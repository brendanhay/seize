#define _POSIX_C_SOURCE 200809L
#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <signal.h>
#include <sys/wait.h>
#include <sys/select.h>
#include <unistd.h>
#include <ei.h>

/* erlang packet header size (contains packet size information) */
#define ERL_HEADER_SIZE 2

/* fd read buffer size */
#define READ_BUF_SIZE 64

typedef enum {
  ARGS_ERR = 221,
  PIPE_ERR,
  FORK_ERR,
  EXEC_ERR,
  WAIT_ERR,
  READ_ERR,
  BIN_ERR
} error_code_t;

char *parse_error(const error_code_t code)
{
  char *text;

  switch (code) {
    case ARGS_ERR:
      text = "incorrect number of arguments";
      break;
    case PIPE_ERR:
      text = "failed to open pipe";
      break;
    case FORK_ERR:
      text = "failed to fork child";
      break;
    case EXEC_ERR:
      text = "failed to execute command";
      break;
    case WAIT_ERR:
      text = "failed to wait for child exit status";
      break;
    case READ_ERR:
      text = "failed to read child fd";
      break;
    case BIN_ERR:
      text = "binary to execute not found";
      break;
    default:
      text = "";
      break;
  }

  return text;
}

void write_packet(char *buf, int sz, FILE *fd)
{
  uint8_t hd[ERL_HEADER_SIZE];

  /* the packet header must be in network byte order */
  hd[0] = (sz >> 8) & 0xff;
  hd[1] = sz & 0xff;

  fwrite(hd, 1, ERL_HEADER_SIZE, fd);
  fwrite(buf, 1, sz, fd);
  fflush(fd);
}

void write_error(const error_code_t code)
{
  ei_x_buff buf;

  /* initialize the state and the output buffer */
  ei_x_new_with_version(&buf);

  /* {error, ..., ...} */
  ei_x_encode_tuple_header(&buf, 3);
  ei_x_encode_atom(&buf, "error");

  /* exit_code */
  ei_x_encode_long(&buf, code);

  /* text string describing c error, or blank */
  ei_x_encode_string(&buf, parse_error(code));

  /* output result */
  write_packet(buf.buff, buf.buffsz, stdout);

  ei_x_free(&buf);
}

void handle_error(const error_code_t code)
{
  /* write the error to stdout using ei */
  write_error(code);

  /* terminate by parent - see exit vs _exit */
  exit(code);
}

void write_data(const char *name, const char *p, int len)
{
  ei_x_buff buf;

  /* initialize the state and the output buffer */
  ei_x_new_with_version(&buf);

  /* {..., ...} */
  ei_x_encode_tuple_header(&buf, 2);
  ei_x_encode_atom(&buf, name);
  ei_x_encode_binary(&buf, p, len);

  /* output result */
  write_packet(buf.buff, buf.buffsz, stdout);

  ei_x_free(&buf);
}

int read_pipe(const char *name, int fd)
{
  char buf[READ_BUF_SIZE];
  int  io;

  if ((io = read(fd, buf, READ_BUF_SIZE)) > 0)
    write_data(name, buf, io);

  return io;
}

/* calculate the highest fd+1, for p/select num of fds */
int max_fd(int out_fd, int err_fd)
{
  if (out_fd > err_fd)
    return out_fd + 1;

  return err_fd + 1;
}

/* SIGTERM handler */
void sigterm(int signo)
{
  (void)signo;
}

struct sigaction setup_sigaction(sigset_t *sigset, sigset_t *oldset)
{
  struct sigaction s;

  /* setup sighandler for SIGTERM */
  s.sa_handler = sigterm;
  sigemptyset(&s.sa_mask);
  s.sa_flags = 0;
  sigaction(SIGTERM, &s, NULL);

  /* block SIGTERM */
  sigemptyset(sigset);
  sigaddset(sigset, SIGTERM);
  sigprocmask(SIG_BLOCK, sigset, oldset);

  return s;
}

void select_pipes(int out_fd, int err_fd)
{
  struct sigaction s;
  sigset_t sigset;
  sigset_t oldset;
  fd_set   set;
  int      ready;
  int      nfds;

  /* setup the sigaction and block on SIGTERM */
  s = setup_sigaction(&sigset, &oldset);

  /* calc the number of fds to select over */
  nfds = max_fd(out_fd, err_fd);

  while (1) {
    int io = 0;

    /* init the file descriptor set. */
    FD_ZERO(&set);

    /* reset the fds */
    FD_SET(out_fd, &set);
    FD_SET(err_fd, &set);

    /* continue until signaled */
    ready = pselect(nfds, &set, NULL, NULL, NULL, &oldset);

    /* pselect error or no data */
    if (ready <= 0)
      break;

    /* check stdout */
    if (FD_ISSET(out_fd, &set))
      io += read_pipe("stdout", out_fd);

    /* check stderr */
    if (FD_ISSET(err_fd, &set))
      io += read_pipe("stderr", err_fd);

    /* no data on either pipe or EINTR */
    if (io == 0 || errno == EINTR)
      break;
  }
}

/* sugar to make sense of which end of the pipe is which */
#define CHILD_OUT_READ  out_pipe[0]
#define CHILD_OUT_WRITE out_pipe[1]
#define CHILD_ERR_READ  err_pipe[0]
#define CHILD_ERR_WRITE err_pipe[1]

int main(int argc, char **argv)
{
  int   out_pipe[2];
  int   err_pipe[2];
  pid_t child;

  /* check the argv length, 1=file, 2...=args */
  if (argc < 2)
    handle_error(ARGS_ERR);

  /* check the requested binary can be found, and executed */
  if (access(argv[1], X_OK) != 0)
    handle_error(BIN_ERR);

  /* create pipe to capture child stdout */
  if (pipe(out_pipe) < 0)
    handle_error(PIPE_ERR);

  /* create pipe to capture child stderr */
  if (pipe(err_pipe) < 0) {
    close(CHILD_OUT_READ);
    close(CHILD_OUT_WRITE);

    handle_error(PIPE_ERR);
  }

  /* fork */
  if ((child = fork()) < 0) {
    /* FATAL: cannot fork child */
    handle_error(FORK_ERR);
  } else if (child == 0) {
    /* child */
    close(STDOUT_FILENO);
    close(STDERR_FILENO);

    /* assign the write ends of the pipe to the child */
    dup2(CHILD_OUT_WRITE, STDOUT_FILENO);
    dup2(CHILD_ERR_WRITE, STDERR_FILENO);

    /* close the pipe ends in the child */
    close(CHILD_OUT_READ);
    close(CHILD_OUT_WRITE);
    close(CHILD_ERR_READ);
    close(CHILD_ERR_WRITE);

    /* exec */
    execv(argv[1], argv + 1);

    _exit(EXEC_ERR);
  } else {
    int status;

    /* close the write ends of the pipes being used by the child */
    close(CHILD_OUT_WRITE);
    close(CHILD_ERR_WRITE);

    /* read all available bytes from the child fds */
    select_pipes(CHILD_OUT_READ, CHILD_ERR_READ);

    /* close the read end of the pipes */
    close(CHILD_OUT_READ);
    close(CHILD_ERR_READ);

    /* wait for the child and grab it's exit status */
    waitpid(child, &status, 0);

    /* need to propogate this to the unforked process */
    if (WIFEXITED(status))
      exit(WEXITSTATUS(status));
    else
      exit(WAIT_ERR);
  }

  return 0;
}
