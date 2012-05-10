#!/usr/sbin/dtrace -qs

 string names[int];

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

/* zone_destroy */
	state[9] = "S09"; /* Destroy started */
/* zone_create */
	state[10] = "S10"; /* Create started */
	state[10] = "S11"; /* Create started */
}

zone_status_set:entry
/ (args[0]->zone_id) != 0 /
{
        printf("%s: %s\n",state[args[1]], stringof(args[0]->zone_name));
}

zone_destroy:entry
/ args[0] != 0 /
{
	printf("%s: %s\n", state[9], names[args[0]]);
}

fop_mkdir:entry
/ stringof(args[0]->v_path) == "/zones" /
{
	printf("%s:%s", state[10], stringof(args[1]));
}

fop_rmdir:entry
/ stringof(args[0]->v_path) == "/zones" /
{
	printf("%s:%s", state[11], stringof(args[1]));
}


fbt:genunix:zone_find_all_by_id:return
/ args[1]->zone_id != 0 /
{
        names[args[1]->zone_id] = stringof(args[1]->zone_name);
}
