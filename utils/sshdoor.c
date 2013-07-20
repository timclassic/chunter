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
#include <zdoor.h>
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

zdoor_result_t *server(zdoor_cookie_t *cookie, char *argp, size_t arpg_sz)
{
  zdoor_result_t *result;
  fprintf(stdout, "%s\n", argp);
  fflush(stdout);
#ifdef DEBUG
  fprintf(stderr, "< %s\r\n", argp);
  fflush(stderr);
#endif
  result = malloc(sizeof(zdoor_result_t));
  result->zdr_size = 3;
  result->zdr_data = malloc(result->zdr_size);
  result->zdr_data[1] = 13;
  result->zdr_data[2] = 0;
  if (getc(stdin) == 49) {
#ifdef DEBUG
    fprintf(stderr, "Allowing access.\r\n");
#endif
    result->zdr_data[0] = 49;
  } else {
#ifdef DEBUG
    fprintf(stderr, "Forbidding access.\r\n");
#endif
    result->zdr_data[0] = 48;
  }

#ifdef DEBUG
  fprintf(stderr, "> %p, %p, %d\r\n", result, result->zdr_data, result->zdr_size);
#endif
  return result;
}

int
main(int argc, char *argv[])
{
  struct stat buf;

	zdoor_handle_t zdid = zdoor_handle_init();

#ifdef DEBUG
  fprintf(stderr, "opening door in zone %s and service %s.\r\n", argv[1], argv[2]);
  fflush(stderr);
#endif

  if (zdoor_open(zdid, argv[1], argv[2], "nomnom", server) < 0){
    exit(1);
  }

  //while (getc(stdin) != EOF);
 	pause(); 
  zdoor_handle_destroy(zdid);
}
