/*
 * This utility provides a SSH door for SmartOS VM's
 * it is based on MerlinDMC's code for a always open
 * SSH door but instead of always allowing access it
 * prints the information to stdout and then reads
 * the response from STDIN (one line).

 * usage: ./zonedoor <uuid> <service>
 * i.e.: ./zonedoor <uuid> _joyent_sshd_key_is_authorized
 * gets: /zones/<uuid>/root/var/tmp/._joyent_sshd_key_is_authorized
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
  fprintf(stderr, "[zonedoor] < %s\r\n", argp);
  fflush(stderr);
#endif
  result = malloc(sizeof(zdoor_result_t));
  result->zdr_data = NULL;
  result->zdr_size = 0;
  if (getline(&(result->zdr_data), &(result->zdr_size), stdin) != EOF) {
#ifdef DEBUG
    fprintf(stderr, "[zonedoor] > ~s\r\n", result->zdr_data);
    fprintf(stderr, "[zonedoor] > %p, %p, %d\r\n", result, result->zdr_data, result->zdr_size);
#endif
    return result;
  } else {
#ifdef DEBUG
    fprintf(stderr, "[zonedoor] Exiting with EOF.");
#endif
    exit(0);
  }
}

int
main(int argc, char *argv[])
{
  struct stat buf;

  zdoor_handle_t zdid = zdoor_handle_init();

#ifdef DEBUG
  fprintf(stderr, "[zonedoor] opening door in zone %s and service %s.\r\n", argv[1], argv[2]);
  fflush(stderr);
#endif

  if (zdoor_open(zdid, argv[1], argv[2], "nomnom", server) < 0){
    exit(1);
  }

  //while (getc(stdin) != EOF);
  pause();
  zdoor_handle_destroy(zdid);
}
