#!/bin/sh 

# crontab -e, then add below
# */2 * * * * /Users/pro/battery-check.sh > /dev/null 2>&1

MODE=`/usr/bin/pmset -g | awk '/hibernatemode/ { print $2 }'` 
LEFT=`/usr/bin/pmset -g batt | grep Internal | awk '{ print $3 }' | awk -F % '{ print $1 }'`

if [ $LEFT -lt 30 ] && [ $MODE != 3 ] ; then 
  { 
     /usr/bin/logger -t "hibernatemode" "Battery level less than 30%; setting hibernatemode to 3" 
     /usr/bin/pmset -a hibernatemode 3 
  } 
elif  [ $LEFT -gt 50 ] && [ $MODE != 0 ]; then 
  { 
     /usr/bin/logger -t "hibernatemode" "Battery level greater than 50%; setting hibernatemode to 0" 
     /usr/bin/pmset -a hibernatemode 0 
     rm /var/vm/sleepimage 
  } 
fi

if [ $LEFT -lt 18 ] && [[ $(/usr/bin/pmset -g batt | grep 'Battery Power') ]] ; then 
  { 
      pmset sleepnow
  } 
fi


