#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <sys/wait.h>
#include <ei.h>

#define ARGS_ERR 221
#define PIPE_ERR 222
#define FORK_ERR 223
#define EXEC_ERR 224
#define WAIT_ERR 225

#define BUFSIZE 1024

#define CHILDOUT_READ  out_pipe[0]
#define CHILDOUT_WRITE out_pipe[1]
#define CHILDERR_READ  err_pipe[0]
#define CHILDERR_WRITE err_pipe[1]

void write_packet(char *buf, int sz, FILE *fd)
{
  uint8_t hd[4];

  /* the packet header must be in network byte order */
  hd[0] = (sz >> 24) & 0xff;
  hd[1] = (sz >> 16) & 0xff;
  hd[2] = (sz >> 8) & 0xff;
  hd[3] = sz & 0xff;

  fwrite(hd, 1, 4, fd);
  fwrite(buf, 1, sz, fd);
  fflush(fd);
}

char *error_text(const long status)
{
  char *text;

  switch (status) {
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
      text = "failed to exec command";
      break;
    case WAIT_ERR:
      text = "failed to wait for child exit status";
      break;
    default:
      text = "";
      break;
  }

  return text;
}

void send_response(const long status, const char *out, const char *err)
{
  char *text;
  ei_x_buff buf;

  /* initialize the state and the output buffer */
  ei_x_new_with_version(&buf);

  /* {ok, ..., ...} tuple */
  ei_x_encode_tuple_header(&buf, 4);
  ei_x_encode_atom(&buf, (status == 0 ? "ok" : "error"));

  /* parse exit code */
  text = error_text(status);

  if (strlen(text) < 1)
    ei_x_encode_long(&buf, status);
  else
    ei_x_encode_string(&buf, text);

  /* {stdout, ...} tuple */
  ei_x_encode_tuple_header(&buf, 2);
  ei_x_encode_atom(&buf, "stdout");
  ei_x_encode_string(&buf, out);

  /* {stderr, ...} tuple */
  ei_x_encode_tuple_header(&buf, 2);
  ei_x_encode_atom(&buf, "stderr");
  ei_x_encode_string(&buf, err);

  /* output result */
  write_packet(buf.buff, buf.buffsz, stdout);

  ei_x_free(&buf);
}

void handle_error(const long status)
{
  char *blank = "";

  send_response(status, blank, blank);
  exit(status);
}

int main(int argc, char **argv)
{
  int out_pipe[2];
  int err_pipe[2];
  pid_t child;

  /* check the argv length, 1=file, 2=search_path, 3...=args */
  if (argc < 3)
    handle_error(ARGS_ERR);

  /* create pipe to capture child stdout */
  if (pipe(out_pipe) < 0)
    handle_error(PIPE_ERR);

  /* create pipe to capture child stderr */
  if (pipe(err_pipe) < 0) {
    close(CHILDOUT_READ);
    close(CHILDOUT_WRITE);
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
    dup2(CHILDOUT_WRITE, STDOUT_FILENO);
    dup2(CHILDERR_WRITE, STDERR_FILENO);

    /* close the pipe ends in the child */
    close(CHILDOUT_READ);
    close(CHILDOUT_WRITE);
    close(CHILDERR_READ);
    close(CHILDERR_WRITE);

    /* TODO: correct parse incoming argv for *rest args */
    char *args[] = {
      argv[1],
      argv[2],
      NULL
    };

    /* exec */
    execv(args[0], (char**)args);

    _exit(EXEC_ERR);
  } else {
    int status;
    char out[BUFSIZ];
    char err[BUFSIZ];

    /* close the write ends of the pipes being used by the child */
    close(CHILDOUT_WRITE);
    close(CHILDERR_WRITE);

    /* read from child read pipes into variables, and terminate the string */
    out[read(CHILDOUT_READ, out, sizeof(out))] = '\0';
    err[read(CHILDERR_READ, err, sizeof(err))] = '\0';

    /* close the read end of the pipes */
    close(CHILDOUT_READ);
    close(CHILDERR_READ);

    wait(&status);

    if (WIFEXITED(status)) {
      send_response(WEXITSTATUS(status), out, err);
    } else {
      handle_error(WAIT_ERR);
    }
  }

  return 0;
}
