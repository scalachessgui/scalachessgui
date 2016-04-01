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
	val enginedata:Data=null
)
{
	var path:String=""
	var protocol:String="UCI"

	val protocols=List("UCI","XBOARD")

	var engineprocess:Process=null
	var enginein:InputStream=null
	var engineout:OutputStream=null
	var enginereadthread:Thread=null

	FromData(enginedata)

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
	}

	def ProcessEngineOut(line:String)
	{
		println(line)
	}

	def IssueCommand(command:String)
	{
		println("issuecommand "+command)
		try
		{
			engineout.write(command.getBytes())
			engineout.flush()
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
				val chunk:Char=enginein.read().toChar
				if(chunk=='\n')
				{
					val len=buffer.length
					if(len>0)
					{
						if(buffer(len-1)=='\r')
						{
							buffer=buffer.substring(0,len-1)
						}
					}
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
					println(s"engine read IO exception, id: $id, path: $path")
				}
			}
		}
	}})}

	def ProtocolStartup
	{
		if(protocol=="UCI")
		{
			IssueCommand("uci\n")
		}

		if(protocol=="XBOARD")
		{
			IssueCommand("xboard\n")
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
        ProtocolStartup
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
		}
	}

	def ToData:Data=
	{
		var mapdata=Map[String,Data]()
		mapdata+=("path" -> StringData(path))
		mapdata+=("protocol" -> StringData(protocol))
		MapData(mapdata)
	}

	def ReportHTML:String=
	{
		val name=ParseEngineNameFromPath(path)
		val protocolselect=protocols.map(p =>{
			val style=if(p==protocol) "background-color: #afffaf; border-style: solid; border-width: 1px; border-color: #afafaf; border-radius: 5px; padding: 3px;" else
				"padding: 4px;"
			s"""<span style='$style; cursor: pointer; font-size: 10px;' onmousedown="idstr='$id'; command='protocolselected'; param='$p';">$p</span>"""
		}).mkString("\n")
		val status=if(engineprocess==null) "<font color='red'>not active</font>" else "<font color='green'>active</font>"
		s"""
			|<div style="border-width: 2px; border-style: dotted; border-color: #afafaf; border-radius: 10px; margin: 3px;">
			|<table>
			|<tr><td>
			|<table>
			|<tr>
			|<td>name</td>
			|<td><font color="red">$name</td>
			|<td><input type="button" value="Load" onclick="idstr='$id'; command='load';"></td>
			|<td><input type="button" value="Unload" onclick="idstr='$id'; command='unload';"></td>
			|<td>status</td>
			|<td>$status</td>
			|<td>protocol</td>
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
			|</tr>
			|</table>
			|</td></tr>
			|<tr><td>
			|<table>
			|<tr>
			|<td><input type="button" value="..." onclick="idstr='$id'; command='editpath';"></td>
			|<td>path</td><td><font color='blue'>$path</font></td>
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

		if(command=="add")
		{
			enginelist=enginelist:+GEngine(enginelist.length)
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

	def Load
	{
		enginelist=Array[GEngine]()
		val enginelistdata=Builder.getcveval("enginelist").asInstanceOf[SeqData]
		if(enginelistdata != null)
		{
			var i= -1
			enginelist=for(enginedata<-enginelistdata.seq.toArray) yield  { i+=1; GEngine(i,enginedata) }
		}
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
			|<script>
			|var command='';
			|var idstr='0';
			|var param='';
			|</script>
			|Under construction!<br>
			|<input type="button" value="Add new engine" onclick="command='add';""><br>
			|$listhtml
		""".stripMargin
	}
}

////////////////////////////////////////////////////////////////////