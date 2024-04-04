F = -lgtkada -lgtkada_gtk -lgtk-3 -lgdk-3 -lglib-2.0

all: main

main: mainwin.adb
	#gnatmake main.adb
	gnatmake -I./gtkada/src/generated -I./gtkada/src mainwin.adb $(F) 

clean:
	rm b~*
	rm *.o
	rm *.ali
	rm main
