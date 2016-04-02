package gui

////////////////////////////////////////////////////////////////////

import javafx.application._
import javafx.stage._
import javafx.scene._
import javafx.scene.layout._
import javafx.scene.control._
import javafx.scene.canvas._
import javafx.scene.input._
import javafx.scene.paint._
import javafx.scene.text._
import javafx.scene.web._
import javafx.scene.image._
import javafx.event._
import javafx.geometry._
import javafx.beans.value._
import javafx.concurrent.Worker._

import java.io._

import data._
import builder._
import settings._

////////////////////////////////////////////////////////////////////

case class GEngine(
	var id:Int=0,
	val enginedata:Data=null,
	val set_handler:Builder.THandler=null
)
{
	var path:String=""
	var protocol:String="UCI"

	val protocols=List("UCI","XBOARD")

	var engineprocess:Process=null
	var enginein:InputStream=null
	var engineout:OutputStream=null
	var enginereadthread:Thread=null

	var autoload=false

	val globalhandler=set_handler

	FromData(enginedata)

	def handler(ev:MyEvent)
	{
		globalhandler(ev)

		if(ev.kind=="button pressed")
		{
			if(ev.id==s"$path#issueenginecommand")
			{
				IssueConsoleEngineCommand
			}
		}

		if(ev.kind=="textfield entered")
		{
			if(ev.id==s"$path#enginecommand")
			{
				IssueConsoleEngineCommand
			}
		}
	}

	def IssueConsoleEngineCommand
	{
		val etext=Builder.gettext(s"$path#enginecommand")
		val command=etext.getText
		etext.setText("")
		IssueCommand(command)
	}

	def Console
	{
		if(Builder.stages.contains(path))
		{
			Builder.closeStage(path)
		} else {
			val blob=s"""
				|<scrollpane>
				|<tabpane>
				|<tab caption="Search output">
				|<label text="search"/>
				|</tab>
				|<tab caption="Console">
				|<vbox padding="5" gap="5">
				|<hbox padding="5" gap="5">
				|<textfield style="-fx-font-size: 18px; -fx-text-fill: #00007f;" id="$path#enginecommand"/>
				|<button id="$path#issueenginecommand" text="Issue" style="round"/>
				|</hbox>
				|<scrollpane id="engineconsolescrollpane" width="800">
				|<webview id="$path#engineconsoletext" height="600" width="3000"/>
				|</scrollpane>
				|</vbox>
				|</tab>
				|<tab caption="Settings">
				|<label text="settings"/>
				|</tab>
				|</tabpane>
				|</scrollpane>
			""".stripMargin
			Builder.MyStage(path,modal=false,set_handler=handler,title=ParseEngineNameFromPath(path)+" console",blob=blob)
			log.Update
		}
	}

	def Unload
	{
		if(enginereadthread!=null)
		{
			enginereadthread.interrupt()
			enginereadthread=null
		}
		if(engineprocess!=null)
		{
			engineprocess.destroy()
			engineprocess=null
		}
		enginein=null
		engineout=null
		println(s"engine $path unloaded")
	}

	def ProcessEngineOut(line:String)
	{
		log.Add(LogItem(line,"out"))
	}

	case class LogItem(line:String,kind:String)
	{
		def ReportHTML:String=
		{
			val color=if(kind=="in") "#ff0000" else "#0000ff"
			val mline=line.replaceAll("\\n","<br>")
			s"""
				|<font color="$color">$mline</font>
			""".stripMargin
		}
	}

	def UpdateConsoleLog(content:String)
	{
		val cwe=Builder.getwebe(s"$path#engineconsoletext")
		if(cwe==null) return
		cwe.loadContent(content)
	}

	case class Log(buffersize:Int=200)
	{
		var Items=List[LogItem]()

		def Update
		{
			Platform.runLater(new Runnable{def run{
				UpdateConsoleLog(ReportHTML)
			}})
		}

		def Add(item:LogItem)
		{
			Items=Items:+item
			while(Items.length>buffersize) Items=Items.tail
			Update
		}

		def ReportHTML:String=
		{
			val itemshtml=Items.reverse.map(item => item.ReportHTML).mkString("<br>\n")
			s"""
				|<div style="font-family: monospace;">
				|$itemshtml
				|</div>
			""".stripMargin
		}
	}

	var log=Log()

	def IssueCommand(command:String)
	{
		if(command==null) return
		val fullcommand=command+"\n"
		try
		{
			engineout.write(fullcommand.getBytes())
			engineout.flush()
			log.Add(LogItem(command,"in"))
		}
		catch
		{
			case e: Throwable =>
			{
				println(s"engine write IO exception, command: $command, id: $id, path: $path")
			}
		}
	}

	def CreateEngineReadThread:Thread={new Thread(new Runnable{def run{
		var buffer=""
		while (!Thread.currentThread().isInterrupted()){
			try
			{
				val chunkobj=enginein.read()
				try
				{ 
					val chunk=chunkobj.toChar
					if(chunk=='\n')
					{						
						ProcessEngineOut(buffer)
						buffer=""
					} else {
						buffer+=chunk
					}
				}
				catch
				{
					case e: Throwable => 
					{
						println(s"engine read not a char exception, chunk: $chunkobj, id: $id, path: $path")
					}
				}
			}
			catch
			{
				case e: Throwable =>
				{
					println(s"engine read IO exception, id: $id, path: $path")
				}
			}
		}
	}})}

	def ProtocolStartup
	{
		if(protocol=="UCI")
		{
			IssueCommand("uci")
		}

		if(protocol=="XBOARD")
		{
			IssueCommand("xboard")
		}
	}

	def Load:Boolean=
	{
		Unload
		val processbuilder=new ProcessBuilder(path)
		val epf=new File(path)
		if(epf.exists())
		{
			processbuilder.directory(new File(epf.getParent()))
		}
		else
		{
			return false
		}
		try
		{
			engineprocess=processbuilder.start()
		}
		catch
		{
			case e: Throwable =>
			{
				return false
			}
		}
		enginein=engineprocess.getInputStream()
        engineout=engineprocess.getOutputStream()
        enginereadthread=CreateEngineReadThread
        enginereadthread.start()
        Thread.sleep(100)
        ProtocolStartup
        println(s"engine $path loaded")
		return true
	}

	def ParseEngineNameFromPath(path:String):String=
	{
		val f=new File(path)
		var engine_name=""
		if(f.exists)
		{
			val engine_full_name=f.getName()
			val engine_full_name_parts=engine_full_name.split("\\.").toList
			engine_name=engine_full_name_parts.head
		}
		engine_name
	}

	def SetId(setid:Int):GEngine=
	{
		id=setid
		return this
	}

	def FromData(enginedata:Data)
	{
		if(enginedata == null) return

		if(enginedata.isInstanceOf[MapData])
		{
			val enginemapdata=enginedata.asInstanceOf[MapData]

			val pathdata=Data.get(enginemapdata,"path")

			if(pathdata != null)
			{
				if(pathdata.isInstanceOf[StringData])
				{
					path=pathdata.asInstanceOf[StringData].value
				}
			}

			val protocoldata=Data.get(enginemapdata,"protocol")

			if(protocoldata != null)
			{
				if(protocoldata.isInstanceOf[StringData])
				{
					protocol=protocoldata.asInstanceOf[StringData].value
				}
			}

			val autoloaddata=Data.get(enginemapdata,"autoload")

			if(autoloaddata != null)
			{
				if(autoloaddata.isInstanceOf[StringData])
				{
					autoload=false
					if(autoloaddata.asInstanceOf[StringData].value=="true")
					{
						autoload=true
					}
				}
			}
		}
	}

	def ToData:Data=
	{
		var mapdata=Map[String,Data]()
		mapdata+=("path" -> StringData(path))
		mapdata+=("protocol" -> StringData(protocol))
		mapdata+=("autoload" -> StringData(""+autoload))
		MapData(mapdata)
	}

	def SwitchAutoload
	{
		autoload= !autoload
	}

	def ReportHTML:String=
	{
		val name=ParseEngineNameFromPath(path)
		val protocolselect=protocols.map(p =>{
			val style=if(p==protocol) "background-color: #ffffaf; border-style: solid; border-width: 1px; border-color: #afafaf; border-radius: 5px; padding: 3px;" else
				"padding: 4px;"
			s"""<span style='$style; cursor: pointer; font-size: 10px;' onmousedown="idstr='$id'; command='protocolselected'; param='$p';">$p</span>"""
		}).mkString("\n")
		val status=if(engineprocess==null) "<font color='red'>not active</font>" else "<font color='green'>active</font>"
		val autoloadbackground=if(autoload) "#ffffaf" else "#ffffff"
		val divbackground=if(engineprocess!=null) "#afffaf" else "#ffafaf"
		val autoloadstatus=if(autoload) "On" else "Off"
		val consoleopen=Builder.stages.contains(path)
		val consoletext=if(consoleopen) "Close Console/Settings" else "Open Console/Settings"
		s"""
			|<div style="background-color: $divbackground; border-width: 2px; border-style: dotted; border-color: #afafaf; border-radius: 10px; margin: 3px;">
			|<table>
			|<tr><td>
			|<table>
			|<tr>
			|<td class="italiclabel">name</td>
			|<td style="padding-left: 3px; padding-right: 3px; border-style: dotted; border-radius: 5px; border-color: #7f7fff; font-size: 20px; font-weight: bold; color: #0000ff">$name</td>
			|<td><input type="button" value="Load" onclick="idstr='$id'; command='load';"></td>
			|<td><span onmousedown="idstr='$id'; command='autoload';" style="cursor: pointer; border-style: solid; border-width: 1px; border-radius: 5px; font-size: 12px; padding-left: 6px; padding-right: 9px; padding-top: 4px; padding-bottom: 4px; background-color: $autoloadbackground;">Auto Load $autoloadstatus</span></td>
			|<td><input type="button" value="Unload" onclick="idstr='$id'; command='unload';"></td>
			|<td class="italiclabel">status</td>
			|<td><span style="border-style: solid; border-color: #000000; border-width: 1px; border-radius: 5px; padding-left: 3px; padding-right: 3px; padding-bottom: 2px;">$status</span></td>
			|<td class="italiclabel">protocol</td>
			|<td>$protocolselect</td>
			|</tr>
			|</table>
			|</td></tr>
			|<tr><td>
			|<table>
			|<tr>
			|<td><input type="button" value="To top" onclick="idstr='$id'; command='top';"></td>
			|<td><input type="button" value="Up" onclick="idstr='$id'; command='up';"></td>
			|<td><input type="button" value="Down" onclick="idstr='$id'; command='down';"></td>
			|<td><input type="button" value="To bottom" onclick="idstr='$id'; command='bottom';"></td>
			|<td><input type="button" value="$consoletext" onclick="idstr='$id'; command='console';"></td>
			|</tr>
			|</table>
			|</td></tr>
			|<tr><td>
			|<table>
			|<tr>
			|<td><input type="button" value="..." onclick="idstr='$id'; command='editpath';"></td>
			|<td class="italiclabel">path</td><td><font color='blue'>$path</font></td>
			|<td><input type="button" value="Delete Engine" onclick="idstr='$id'; command='del';"></td>
			|</tr>
			|</table>
			|</td></tr>
			|</table>
			|</div>
		""".stripMargin
	}
}

case class GEngineList(var we:WebEngine=null)
{
	var enginelist=Array[GEngine]()

	def handler(ev:MyEvent)
	{
		if(ev.kind=="stage closed")
		{
			Update
		}
	}

	def BrowsePath(id:Int)
	{
		val fc=new FileChooser()

		if(new File(settings.engine_dir).exists)
		{
			fc.setInitialDirectory(new File(settings.engine_dir))
		}

		val f=fc.showOpenDialog(new Stage())

		if(f!=null)
		{

			val dir=f.getParent()

			settings.engine_dir=dir

			val path=f.getPath()

			enginelist(id).path=path

		}
	}

	def Del(id:Int)
	{
		var i= -1
		var j= -1
		enginelist=for(engine <- enginelist; if({ i+=1; i != id })) yield { j+=1; engine.SetId(j) }
	}

	def Move(id:Int,dir:Int):Boolean=
	{
		val last=enginelist.length-1
		if(((id==0)&&(dir== -1))||((id==last)&&(dir==1))) return false
		val temp=enginelist(id)
		enginelist(id)=enginelist(id+dir).SetId(id)
		enginelist(id+dir)=temp.SetId(id+dir)
		true
	}

	def ToEdge(id:Int,dir:Int)
	{
		var i=id
		while(Move(i,dir)){i+=dir}
	}

	def Handle
	{
		val command=we.executeScript("command").toString()
		val idstr=we.executeScript("idstr").toString()
		val param=we.executeScript("param").toString()

		if(command=="console")
		{
			enginelist(idstr.toInt).Console
			Update
		}

		if(command=="add")
		{
			enginelist=enginelist:+GEngine(enginelist.length,set_handler=handler)
			Update
		}

		if(command=="top")
		{
			ToEdge(idstr.toInt,-1)
			Update
		}

		if(command=="up")
		{
			Move(idstr.toInt,-1)
			Update
		}

		if(command=="down")
		{
			Move(idstr.toInt,1)
			Update
		}

		if(command=="bottom")
		{
			ToEdge(idstr.toInt,1)
			Update
		}

		if(command=="editpath")
		{
			BrowsePath(idstr.toInt)
			Update
		}

		if(command=="del")
		{
			Del(idstr.toInt)
			Update
		}

		if(command=="protocolselected")
		{
			enginelist(idstr.toInt).protocol=param
			Update
		}

		if(command=="load")
		{
			enginelist(idstr.toInt).Load
			Update
		}

		if(command=="unload")
		{
			enginelist(idstr.toInt).Unload
			Update
		}

		if(command=="autoload")
		{
			enginelist(idstr.toInt).SwitchAutoload
			Update
		}
	}

	def Update
	{
		Save
		val content=ReportHTML
		val st=we.executeScript("document.body.scrollTop").toString().toDouble
		we.loadContent(content)
		we.getLoadWorker().stateProperty().addListener(new ChangeListener[State]{
	        def changed(ov: ObservableValue[_ <: State], oldState: State, newState: State)
	        {
                if (newState == State.SUCCEEDED)
                {
                	we.executeScript("window.scrollTo(" + 0 + ", " + st + ")");
				}
			}
		})
	}

	def UnloadAll
	{
		for(engine<-enginelist)
		{
			engine.Unload
		}
	}

	def LoadAllAuto
	{
		for(engine<-enginelist)
		{
			if(engine.autoload) engine.Load
		}
	}

	def Load
	{
		UnloadAll
		enginelist=Array[GEngine]()
		val enginelistdata=Builder.getcveval("enginelist").asInstanceOf[SeqData]
		if(enginelistdata != null)
		{
			var i= -1
			enginelist=for(enginedata<-enginelistdata.seq.toArray) yield  { i+=1; GEngine(i,enginedata,set_handler=handler) }
		}
		LoadAllAuto
		Update
	}

	def ToData:Data=
	{
		val seqdata=(for(engine <- enginelist) yield engine.ToData).toSeq
		SeqData(seqdata)
	}

	def Save
	{
		Builder.setcveval("enginelist",ToData)
	}

	def ReportHTML:String=
	{
		val listhtml=enginelist.map(e => e.ReportHTML).mkString("\n")
		s"""
			|<html>
			|<head>
			|<style>
			|.italiclabel {
    		|	font-style: italic;
    		|	font-size: 12px;
			|}
			|</style>
			|<script>
			|var command='';
			|var idstr='0';
			|var param='';
			|</script>
			|</head>
			|<body>
			|Under construction!<br>
			|<input type="button" value="Add new engine" onclick="command='add';""><br>
			|$listhtml
			|</body>
			|</html>
		""".stripMargin
	}
}

////////////////////////////////////////////////////////////////////