#!/usr/sbin/dtrace -qs

BEGIN
{
/* zone_status_set */
        state[0] = "S00"; /* UNINITIALIZED */
        state[1] = "S01"; /* INITIALIZED */
        state[2] = "S02"; /* READY */
        state[3] = "S03"; /* BOOTING */
        state[4] = "S04"; /* RUNNING */
        state[5] = "S05"; /* SHUTTING_DOWN */
        state[6] = "S06"; /* EMPTY */
        state[7] = "S07"; /* DOWN */
        state[8] = "S08"; /* DYING */
        state[9] = "S09"; /* DEAD */
/* zone_create */
	state[10] = "S10"; /* Create started */
	state[11] = "S11"; /* Delete started */
}

zone_status_set:entry
/ (args[0]->zone_id) != 0 /
{
        printf("%s: %s\n",state[args[1]], stringof(args[0]->zone_name));
}

fop_mkdir:entry
/ stringof(args[0]->v_path) == "/zones" /
{
	printf("%s: %s", state[9], stringof(args[1]));
}

fop_rmdir:entry
/ stringof(args[0]->v_path) == "/zones" /
{
	printf("%s: %s", state[10], stringof(args[1]));
}
