#!/usr/sbin/dtrace -qs

BEGIN
{
/* zone_status_set */
        state[0] = "S00"; /* "Uninitialized"; */
        state[1] = "S01"; /* "Ready"; */
        state[2] = "S02"; /* "Booting"; */
        state[3] = "S03"; /* "Running"; */
        state[4] = "S04"; /* "Shutting down"; */
        state[5] = "S05"; /* "Empty"; */
        state[6] = "S06"; /* "Down"; */
        state[7] = "S07"; /* "Dying"; */
        state[8] = "S08"; /* "Dead"; */
/* zone_create */
	state[9] = "S09"; /* Create started */
	state[10] = "S10"; /* Create started */
}

zone_status_set:entry
/ (args[0]->zone_id) != 0 /
{
        printf("%s: %s\n",state[args[1]], stringof(args[0]->zone_name));
}

fop_mkdir:entry
/ stringof(args[0]->v_path) == "/zones" /
{
	printf("%s: %s", state[09], stringof(args[1]));
}

fop_rmdir:entry
/ stringof(args[0]->v_path) == "/zones" /
{
	printf("%s: %s", state[10], stringof(args[1]));
}
