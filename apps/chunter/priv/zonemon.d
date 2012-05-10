#!/usr/sbin/dtrace -qs

BEGIN
{
        state[0] = "Uninitialized";
        state[1] = "Ready";
        state[2] = "Booting";
        state[3] = "Running";
        state[4] = "Shutting down";
        state[5] = "Empty";
        state[6] = "Down";
        state[7] = "Dying";
        state[8] = "Dead";
}

zone_status_set:entry
{
        printf("Zone %s status %s\n", stringof(args[0]->zone_name),
                state[args[1]]);
}

zone_destroy:entry
{
	printf("Zone %s status destroyed\n", stringof(args[0]));
}
