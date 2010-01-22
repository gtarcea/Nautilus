include $(SYGHOME)/code/make/makefile_platforms

MONOLIB := $(sharedlibarea)mono

all : $(binarea)cfgwin.exe

$(binarea)cfgwin.exe : cfgwin.cs
#	mcs -lib:$(MONOLIB) -r:System.Drawing -r:System -pkg:gtk-sharp -pkg:glade-sharp -out:$(@) $(<)

run : $(binarea)cfgwin.exe
	mono $(binarea)cfgwin.exe

clean:
	echo "No clean"
