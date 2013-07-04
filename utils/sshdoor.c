/*
 * This utility provides a SSH door for SmartOS VM's
 * it is based on MerlinDMC's code for a always open
 * SSH door but instead of always allowing access it
 * prints the information to stdout and then read a
 * 1 char response 1 (not "1") if the key should be
 * allowed 0 (not "0") if it should not.
 *
 * usage: ./sshdoor /zones/<uuid>/root/var/tmp/._joyent_sshd_key_is_authorized
 */

#pragma ident "%Z%%M% %I% %E% SMI"

#include <alloca.h>
#include <door.h>
#include <errno.h>
#include <fcntl.h>
#include <pwd.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

void
server(void *cookie, char *argp, size_t arg_size, door_desc_t *dp,
       uint_t n_desc)
{
  fprintf(stdout, "%s\n", argp);
  fflush(stdout),
    if (getc(stdin) == 1)
      door_return("1", 256, NULL, 0); // allow all logins - yay!
    else
      door_return("0", 256, NULL, 0); // allow all logins - yay!
  /* NOTREACHED */
}

int
main(int argc, char *argv[])
{
  int did;
  struct stat buf;

  if ((did = door_create(server, 0, 0)) < 0) {
    perror("door_create");
    exit(1);
  }

  /* make sure file system location exists */
  if (stat(argv[1], &buf) < 0) {
    int newfd;
    if ((newfd = creat(argv[1], 0444)) < 0) {
      perror("creat");
      exit(1);
    }
    (void) close(newfd);
  }

  /* make sure nothing else is attached */
  (void) fdetach(argv[1]);

  /* attach to file system */
  if (fattach(did, argv[1]) < 0) {
    perror("fattach");
    exit(2);
  }

  (void) pause();
}
