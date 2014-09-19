/*
 * This utility provides a SSH door for SmartOS VM's
 * it is based on MerlinDMC's code for a always open
 * SSH door but instead of always allowing access it
 * prints the information to stdout and then reads
 * the response from STDIN (one line).
 *
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

#define HEARTBEAT 7  // number of seconds missing HB before exit


zdoor_result_t *server(zdoor_cookie_t *cookie, char *argp, size_t arpg_sz);
void addVMDoor(char * zoneID, char *doorName, char *doorBiscuit);
void rmVMDoor(char * zoneID, char *doorName);
char *split_space(char *input);
zdoor_handle_t zdid;
int pendingRequest;
char *requestResponse;


void addVMDoor(char *zoneID, char *doorName, char *doorBiscuit){
  char* err = "unknown errror";
  fprintf(stderr, "[zonedoor:%s] opening door '%s' in zone %s.\r\n", doorBiscuit, doorName, zoneID);
  switch (zdoor_open(zdid, zoneID, doorName, doorBiscuit, server)) {
  case 0:
    return;
  case -1:
    err = "General error";
    break;
  case -2:
    err = "Not the global zone";
    break;
  case -3:
    err = "Zone not running";
    break;
  case -4:
    err = "Zone forbidden";
    break;
  case -5:
    err = "argument error";
    break;
  case -6:
    err = "Out of memory";
    break;
  }
  fprintf(stderr, "Error [zonedoor] opening door '%s' in zone %s: %s.\r\n", doorName, zoneID, err);
  //  printf("ok\n");  //no sure if responce is necessary. right now chunter_vm_auth does not handle it.
  //  fflush(stdout);
}

void deleteVMDoor(char *zoneID, char *doorName){
  zdoor_close(zdid, zoneID, doorName);
  // printf("ok\n");
  // fflush(stdout);

}

zdoor_result_t *server(zdoor_cookie_t *cookie, char *argp, size_t arpg_sz)
{
  zdoor_result_t *result;
  fprintf(stdout, "%s %s\n", cookie->zdc_biscuit, argp);
  fflush(stdout);
  pendingRequest = 1;
  char deny[] = "0timeout";

  result = malloc(sizeof(zdoor_result_t));
  result->zdr_data = NULL;
  result->zdr_size = 9;

  int i = 0;
  while(i<500) {
    if(pendingRequest == 2){
      result->zdr_data = requestResponse;
      result->zdr_size = strlen(result->zdr_data);
      pendingRequest = 0;
      return result;
    }
    i++;
    nanosleep((struct timespec[]){{0, 100000000}}, NULL);
  }
  fprintf(stderr, "Timeout in reply.\r\n");
  pendingRequest = 0;
  result->zdr_data = deny;
  return result;
}


void sigAlrmHandler(int sig) {
  exit(0);
}


int
main(int argc, char *argv[])
{

  signal(SIGALRM, sigAlrmHandler);
  alarm(HEARTBEAT);

  zdid = zdoor_handle_init();

  while(1){

    size_t nbytes = 0;
    size_t len = 0;
    char *input = NULL;
    char *arg2 = NULL;
    char *arg3 = NULL;

    while(pendingRequest == 2){
      nanosleep((struct timespec[]){{0, 100000000}}, NULL);
    }
    getline(&input, &nbytes, stdin);
    switch(input[0])
      {
      case 'h':    // heartbeat
        alarm(HEARTBEAT);
        break;
      case 'a':    // add zone door
        input++[strlen(input)-1]=0;
        arg2 = input;
        while (*arg2 != ' ' && *arg2 != 0) {
          arg2++;
        };
        if ((arg2 = split_space(input)) &&
            (arg3 = split_space(arg2))) {
          addVMDoor(input, arg2, arg3);
        } else {
          fprintf(stderr, "Invalid input: '%s'.\r\n", input);
        }
        break;
      case 'd':    // delete zone door
        input++[strlen(input)-1]=0;
        if (arg2 = split_space(input)) {
          deleteVMDoor(input, arg2);
        } else {
          fprintf(stderr, "Invalid input: '%s'.\r\n", input);
        }
        break;
      case 'r':   // request response
        if (pendingRequest == 1) {
          len = strlen(input);
          input++[len - 1] = 0;
          requestResponse = malloc(len * sizeof(char));
          strlcpy(requestResponse, input, len);
          pendingRequest = 2;
        }
        break;
      default:
        break;
      }

    free(input);

  }
}

char* split_space(char *input) {
  char* arg = input;
  while (*arg != ' ' && *arg != 0) {
    arg++;
  };
  if (*arg == ' ') {
    *arg = 0;
    arg++;
    return arg;
  } else {
    return NULL;
  }
};
