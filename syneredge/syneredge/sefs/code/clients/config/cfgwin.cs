using System;
using Glade;
using Gtk;

namespace Config
{

class ConfigWin
{
[Widget]Gtk.Entry traceDirectory;
[Widget]Gtk.TextView traceSubsystems;
[Widget]Gtk.Combo traceLevel;
[Widget]Gtk.Button btnCancel;
[Widget]Gtk.Button btnSave;
[Widget]Gtk.Window cfgWindow;

 public ConfigWin()
 {
	Glade.XML gui = new Glade.XML("./cfgwin.glade", "cfgWindow", "");

	gui.Autoconnect(this);
	cfgWindow.DeleteEvent += new DeleteEventHandler(Quit);
	cfgWindow.ShowAll();
 }

void on_btnCancel_clicked(object sender, EventArgs e)
{
	Application.Quit();
}

void on_btnSave_clicked(object sender, EventArgs e)
{
	Console.WriteLine("trace directory: " + traceDirectory.Text);
	Console.WriteLine("trace level: " + traceLevel.Entry.Text);
	Console.WriteLine("trace subsystems: " + traceSubsystems.Buffer.Text);	
}

void Quit(object sender, DeleteEventArgs e)
{
	Application.Quit();
}

}

class Configuration
{
	public static void Main(string[] args)
	{
		Application.Init();

		new ConfigWin();

		Application.Run();
	}
}

}

