cat /usr/include/sys/errno.h | grep -e "^#define[[:space:]]E" | sed 's/[[:space:]]/:/g' | cut -d: -f2

