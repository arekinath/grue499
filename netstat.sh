#!/bin/sh
/server/home/xlex/grue499/netstat -A inet -tlnp | egrep -v 'hadoop|e3mpi|java|mpirun|mpd'
