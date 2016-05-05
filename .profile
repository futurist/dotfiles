# Determine windows path
W=`pwd -W`

# Determine identifying version info
V=`echo -n \`uname -r | cut -d \( -f 1\`-\`uname -v | sed -e 's/ /@/'\``

# Reset bash prompt
export PS1='\[\033[33m\w\033[0m\]$ '

