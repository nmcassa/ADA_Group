with gtk.main;
with gtk.window;

procedure MAINWIN is

	window : gtk.window.gtk_window;

begin
	Gtk.Main.Init;

	gtk.window.gtk_new(window);
	window.set_default_size(900, 600);

	--window.on_destroy();	

	Gtk.Main.Main;
end MAINWIN;
