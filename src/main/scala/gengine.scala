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
import game._

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

	var multipv=1

	def SetMultipv(set_multipv:Int,g:game)
	{
		multipv=set_multipv

		if(protocol=="UCI")
		{
			val wasrunning=running
			if(running) Stop
			if(IsAtomkraft)
			{
				IssueCommand("3\n"+set_multipv)
			} else {
				IssueCommand("setoption name MultiPV value "+multipv)
			}
			if(wasrunning) Start(g)
		}
	}

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

	def CloseConsole
	{
		if(Builder.stages.contains(path))
		{
			Builder.closeStage(path)
		}
	}

	def OpenConsole
	{
		if(engineprocess==null)
		{
			CloseConsole
			return
		}

		if(Builder.stages.contains(path))
		{
			Builder.stages(path).ToTop
			return
		}	

		val blob=s"""
			|<scrollpane>
			|<tabpane>
			|<tab caption="Search output">
			|<scrollpane id="engineoutscrollpane" width="800">
			|<webview id="$path#engineouttext" height="3000" width="3000"/>
			|</scrollpane>
			|</tab>
			|<tab caption="Console">
			|<vbox padding="5" gap="5">
			|<hbox padding="5" gap="5">
			|<textfield style="-fx-font-size: 18px; -fx-text-fill: #00007f;" id="$path#enginecommand"/>
			|<button id="$path#issueenginecommand" text="Issue" style="round"/>
			|</hbox>
			|<scrollpane id="engineconsolescrollpane" width="800">
			|<webview id="$path#engineconsoletext" height="3000" width="3000"/>
			|</scrollpane>
			|</vbox>
			|</tab>
			|<tab caption="Settings">
			|<scrollpane id="enginesettingsscrollpane" width="800">
			|<vbox id="$path#enginesettingsvbox" height="3000" width="3000"/>
			|</scrollpane>
			|</tab>
			|</tabpane>
			|</scrollpane>
		""".stripMargin
		Builder.MyStage(path,modal=false,set_handler=handler,title=ParseEngineNameFromPath(path)+" console",blob=blob)
		BuildOptions
		log.Update
	}

	def Console
	{
		if(engineprocess==null)
		{
			CloseConsole
			return
		}
		if(Builder.stages.contains(path))
		{
			CloseConsole
		} else {
			OpenConsole
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
		CloseConsole
		println(s"engine $path unloaded")
	}

	case class Option(
		var name:String="",
		var kind:String="",
		var minstr:String="",
		var maxstr:String="",
		var defaultstr:String=""
	)
	{
		def Apply
		{
			if(kind!="button")
			{
				val id=s"engineoptions#$path#$name"

				var value=Builder.getcvevals(id,defaultstr)

				if((kind=="spin")&&(value!=defaultstr)) value=""+value.toDouble.toInt

				IssueCommand("setoption name "+name+" value "+value)
			}
		}

		def ParseLine(line:String):Option=
		{
			val tokenizer=Tokenizer(line)
			val head=tokenizer.Poll
			if(head==null) return null

			if(protocol=="UCI")
			{
				if(tokenizer.Get!="option") return null

				val reservedtokens=List("name","type","min","max","default")

				def IsReserved(token:String)=reservedtokens.contains(token)

				while(tokenizer.HasToken)
				{
					var currenttoken=tokenizer.Get

					if(!IsReserved(currenttoken))
					{
						if((name=="")||(kind=="")) return null

						return this
					}

					var fieldbuff=List[String]()

					while(tokenizer.HasToken&&(!IsReserved(tokenizer.Poll)))
					{
						fieldbuff=fieldbuff:+tokenizer.Get
					}

					val field=fieldbuff.mkString(" ")

					if(currenttoken=="name") name=field
					if(currenttoken=="type") kind=field
					if(currenttoken=="min") minstr=field
					if(currenttoken=="max") maxstr=field
					if(currenttoken=="default") defaultstr=field
				}
			}

			return this
		}

		def ReportXML:String=
		{
			var td1=""
			var td2=""
			var td3=""
			val id=s"engineoptions#$path#$name"
			if(kind=="button")
			{
				td1=s"""
					|<button id="$id" text="$name"/>
				""".stripMargin
			}
			if(kind=="check")
			{
				td1=s"""
					|<label text="$name"/>
				""".stripMargin
				var value=Builder.getcvevals(id,defaultstr)
				Builder.setcveval(id,StringData(value))				
				td2=s"""
				|<checkbox id="$id" usevariantentry="true"/>
				""".stripMargin
			}
			if(kind=="string")
			{
				td1=s"""
					|<label text="$name"/>
				""".stripMargin
				var value=Builder.getcvevals(id,defaultstr)
				Builder.setcveval(id,StringData(value))				
				td2=s"""
				|<textfield id="$id" usevariantentry="true"/>
				""".stripMargin
				td3=s"""
					|<button id="$id!apply" text="Apply"/>
				""".stripMargin
			}
			if(kind=="spin")
			{				
				val minv=ParseInt(minstr,0)
				val maxv=ParseInt(maxstr,100)
				var span=maxv-minv
				if(minv==1) span+=1
				var unit=1
				if(span>10)
				{
					unit=span/10
				}
				var value=Builder.getcvevals(id,""+defaultstr.toDouble)
				Builder.setcveval(id,StringData(value))
				td1=s"""
					|<label text="$name"/>
				""".stripMargin
				td2=s"""
				|<slider width="300.0" id="$id" usevariantentry="true" min="$minstr" max="$maxstr" majortickunit="$unit" showticklabels="true"/>
				""".stripMargin
			}
			s"""
				|<hbox padding="3" gap="3">
				|$td1
				|$td2
				|$td3
				|</hbox>
			""".stripMargin
		}
	}

	case class Options(var options:List[Option]=List[Option]())
	{
		def Add(o:Option)
		{
			options=options:+o
		}

		def ReportXML:String=
		{
			val content=(for(option<-options) yield option.ReportXML).mkString("\n")
			content
		}

		def ApplyAll
		{
			for(option<-options) option.Apply
		}
	}

	def GetNameFromId(id:String):String=
	{
		val parts=id.split("#").toList
		parts.reverse.head
	}

	def GetPathFromId(id:String):String=
	{
		val parts=id.split("#").toList
		parts.reverse.tail.reverse.mkString("#")
	}

	def options_handler(ev:MyEvent)
	{
		if(ev.kind=="button pressed")
		{
			val buttonname=GetNameFromId(ev.id)

			val parts=buttonname.split("!").toList

			if((parts.length==2)&&(parts(1)=="apply"))
			{				
				var value=""
				var truename=parts(0)
				val truepath=GetPathFromId(ev.id)+"#"+truename
				val comp=Builder.getcomp(truepath)
				if(comp!=null)
				{
					val guivalue=comp.get_gui_value
					if(guivalue!=null)
					{
						if(guivalue.isInstanceOf[StringData])
						{
							value=guivalue.asInstanceOf[StringData].value
							Builder.setcveval(truepath,StringData(value))
						}
					}
				}
				IssueCommand("setoption name "+truename+" value "+value)
			} else {
				IssueCommand("setoption name "+buttonname+" value ")
			}
		}

		if(ev.kind=="slider changed")
		{
			Builder.setcveval(ev.id,StringData(ev.value))

			val slidername=GetNameFromId(ev.id)

			val sliderint=ev.value.toDouble.toInt

			IssueCommand("setoption name "+slidername+" value "+sliderint)
		}

		if(ev.kind=="checkbox changed")
		{
			Builder.setcveval(ev.id,StringData(ev.value))

			val checkboxname=GetNameFromId(ev.id)

			val checkboxbool=ev.value

			IssueCommand("setoption name "+checkboxname+" value "+checkboxbool)
		}
	}

	def BuildOptions
	{
		val svboxcomp=Builder.getcomp(s"$path#enginesettingsvbox")
		if(svboxcomp==null) return
		val svbox=svboxcomp.node.asInstanceOf[VBox]
		if(svbox==null) return
		val optionscontent=options.ReportXML
		val blob=s"""
			|<vbox padding="3" gap="3">
			|$optionscontent
			|</vbox>
		""".stripMargin
		val scenegraph=Builder.build(s"$path#enginesettingsvboxscenegraph",options_handler,blob=blob)
		svbox.getChildren().clear()
		svbox.getChildren().add(scenegraph)
	}

	def ParseStartup(line:String)
	{
		val tokenizer=Tokenizer(line)

		if(protocol=="UCI")
		{
			val head=tokenizer.Poll
			if(head=="uciok")
			{
				startup=false
				options.ApplyAll
			} else {
				val option=Option().ParseLine(line)
				if(option!=null)
				{					
					options.Add(option)					
				}
			}
		}
	}

	def ProcessEngineOut(line:String)
	{
		log.Add(LogItem(line,"out"))

		if(startup)
		{
			ParseStartup(line)
		}
		else
		{
			thinkingoutput.ParseLine(line)
		}

		if(protocol=="UCI")
		{
			val token=Tokenizer(line).Get
			if(token=="bestmove") bestmovereceived=true
		}
	}

	case class LogItem(line:String,kind:String)
	{
		def ReportHTML:String=
		{
			val color=if(kind=="in") "#ff0000" else "#0000ff"
			var rline=line.replaceAll("<","&lt;")
			rline=rline.replaceAll(">","&gt;")
			rline=rline.replaceAll("\\n","<br>")
			s"""
				|<font color="$color">$rline</font>
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

	var options=Options()

	def ProtocolStartup
	{
		startup=true

		options=Options()

		if(protocol=="UCI")
		{
			IssueCommand("uci")
		}

		if(protocol=="XBOARD")
		{
			IssueCommand("xboard")
		}

		var cnt=0
		while((startup)&&(cnt< 50))
		{
			Thread.sleep(100)
			cnt+=1
		}

		startup=false
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
		var consolebuttontext=s"""
			|<td><input type="button" value="$consoletext" onclick="idstr='$id'; command='console';"></td>
		""".stripMargin
		if(engineprocess==null) consolebuttontext="<td>Tip: for Console/Settings press Load</td>"
		s"""
			|<div style="background-color: $divbackground; border-width: 2px; border-style: dotted; border-color: #afafaf; border-radius: 10px; margin: 3px;">
			|<table>
			|<tr><td>
			|<table>
			|<tr>
			|<td class="italiclabel">name</td>
			|<td style="padding-left: 3px; padding-right: 3px; border-style: dotted; border-radius: 5px; border-color: #7f7fff; font-size: 20px; font-weight: bold; color: #0000ff">$name</td>
			|<td><input type="button" value="Load" onclick="idstr='$id'; command='load';"></td>
			|<td><span onmousedown="idstr='$id'; command='autoload';" style="cursor: pointer; border-style: solid; border-width: 1px; border-radius: 5px; font-size: 12px; padding-left: 6px; padding-right: 9px; padding-top: 4px; padding-bottom: 4px; background-color: $autoloadbackground;">Auto Load</span></td>
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
			|$consolebuttontext
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

	var running=false

	var startup=false

	val clip=Clipboard.getSystemClipboard()

	def IsAtomkraft:Boolean=(ParseEngineNameFromPath(path)=="atomkraft")

	def Start(g:game)
	{
		if(engineprocess==null) return
		if(startup) return
		if(running) return
		thinkingoutput=ThinkingOutput()
		OpenConsole
		if(protocol=="UCI")
		{
			val fen=g.report_fen
			if(IsAtomkraft)
			{
				val content = new ClipboardContent()
                content.putString(fen)
                clip.setContent(content)
                
                IssueCommand("1\n4")
			}
			else
			{
				IssueCommand("position fen "+fen)
				IssueCommand("go infinite")
			}
			
			running=true
		}
	}

	var bestmovereceived=false

	def Stop
	{
		if(engineprocess==null) return
		if(startup) return
		if(!running) return
		if(protocol=="UCI")
		{
			bestmovereceived=false
			if(IsAtomkraft)
			{
				IssueCommand("s")
			} else {
				IssueCommand("stop")
			}
			while(!bestmovereceived)
			{
				Thread.sleep(50)
			}
			running=false
		}
	}

	def CheckRestart(g:game)
	{
		if(engineprocess==null) return
		if(running)
		{
			Stop
			Start(g)
		}
	}

	def Strip(line:String):String=
	{
		var sline=line
		sline=sline.replaceAll("\\r|\\n|^\\s+|\\s+$","")
		sline=sline.replaceAll("\\s+"," ")
		sline
	}

	def Tokens(line:String):List[String]=
	{
		Strip(line).split(" ").toList
	}

	case class Tokenizer(line:String="")
	{
		var tokens=Tokens(line)

		def Get:String=
		{
			if(tokens.length>0)
			{
				val token=tokens.head
				tokens=tokens.tail
				return token
			}
			return null
		}

		def Poll:String=
		{
			if(tokens.length>0)
			{
				val token=tokens.head
				return token
			}
			return null
		}

		def GetRest:String=
		{
			if(tokens.length>0)
			{
				val str=tokens.mkString(" ")
				tokens=List[String]()
				return str
			}
			return null
		}

		def HasToken:Boolean=
		{
			tokens.length>0
		}
	}

	def ParseInt(str:String,default:Int):Int=
	{
		if(str==null) return default
		try
		{
			val intvalue=str.toInt
			return intvalue
		}
		catch{case e: Throwable => {}}
		return default
	}

	case class PvItem(
		var multipv:Int=0,
		var hasmultipv:Boolean=false,
		var depth:Int=0,
		var hasdepth:Boolean=false,
		var nodes:Int=0,
		var nodesverbal:String="",
		var hasnodes:Boolean=false,
		var time:Int=0,
		var hastime:Boolean=false,
		var nps:Int=0,
		var npsverbal:String="",
		var hasnps:Boolean=false,
		var scorestr:String="",
		var scorekind:String="",
		var scorecp:Int=0,
		var scoremate:Int=0,
		var scorenumerical:Int=0,
		var signedscorenumerical:String="",
		var scoreverbal:String="",
		var hasscore:Boolean=false,
		var pv:String="",
		var pvrest:List[String]=List[String](),
		var pvreststr:String="",
		var haspv:Boolean=false,
		var bestmove:String=""
	)
	{
		def AsString:String=
		{
			s"$bestmove $signedscorenumerical depth $depth nodes $nodes nps $nps pv $pvreststr"
		}
		def ParseLine(line:String):PvItem=
		{
			val tokenizer=Tokenizer(line)
			if(protocol=="UCI")
			{
				if(!tokenizer.HasToken) return this
				if(tokenizer.Get!="info") return this
				while(tokenizer.HasToken)
				{
					val name=tokenizer.Get
					if(name=="multipv")
					{
						multipv=ParseInt(tokenizer.Get,multipv)
						hasmultipv=true
					}
					if(name=="score")
					{
						val kind=tokenizer.Get
						val value=ParseInt(tokenizer.Get,if(kind=="mate") scoremate else scorecp)
						scorestr=kind+" "+value
						if(kind=="mate")
						{
							scoremate=value
							if(value>=0)
							{
								scorenumerical=10000-value
							} else {
								scorenumerical= -10000+value
							}
						} else {
							scorecp=value
							scorenumerical=value
						}
						signedscorenumerical=if(scorenumerical>0) "+"+scorenumerical else ""+scorenumerical
						scoreverbal=if(kind=="mate") "mate "+scoremate else signedscorenumerical
						scorekind=kind
						hasscore=true
					}
					if(name=="depth")
					{
						depth=ParseInt(tokenizer.Get,depth)
						hasdepth=true
					}
					def FormatNodes(nodes:Int,unit:Int):String=
					{
						"%.2f".format(nodes.toDouble/unit.toDouble)
					}
					if(name=="nodes")
					{
						nodes=ParseInt(tokenizer.Get,nodes)
						if(nodes< 1000) nodesverbal=""+nodes else
						if(nodes< 1000000) nodesverbal=""+FormatNodes(nodes,1000)+" kN" else nodesverbal=FormatNodes(nodes,1000000)+" MN"
						hasnodes=true
					}
					if(name=="nps")
					{
						nps=ParseInt(tokenizer.Get,nps)
						if(nps< 1000) npsverbal=""+nps else
						if(nps< 1000000) npsverbal=""+FormatNodes(nps,1000)+" kN/s" else npsverbal=FormatNodes(nps,1000000)+" MN/s"
						hasnps=true
					}
					if(name=="time")
					{
						time=ParseInt(tokenizer.Get,time)
						hastime=true
					}
					if(name=="pv")
					{
						pv=tokenizer.GetRest
						if(pv!=null)
						{
							haspv=true
							val pvtokens=Tokens(pv)
							bestmove=pvtokens.head
							pvrest=pvtokens.tail
							pvreststr=pvrest.mkString(" ")
						}
					}
				}
			}
			return this
		}

		def UpdateWith(ui:PvItem):PvItem=
		{
			if(ui.hasmultipv) multipv=ui.multipv ; hasmultipv=true
			if(ui.hasdepth) depth=ui.depth ; hasdepth=true
			if(ui.hasnodes) nodes=ui.nodes ; hasnodes=true
			if(ui.hastime) time=ui.time ; hastime=true
			if(ui.hasnps) nps=ui.nps ; hasnps=true
			if(ui.haspv)
			{
				pv=ui.pv
				pvrest=ui.pvrest
				pvreststr=ui.pvreststr
				bestmove=ui.bestmove
				haspv=true
			}
			if(ui.hasscore)
			{
				scorestr=ui.scorestr
				scorekind=ui.scorekind
				scorecp=ui.scorecp
				scoremate=ui.scoremate
				scorenumerical=ui.scorenumerical
				signedscorenumerical=ui.signedscorenumerical
				scoreverbal=ui.scoreverbal
				nodesverbal=ui.nodesverbal
				npsverbal=ui.npsverbal
				hasscore=true
			}
			return this
		}

		def ReportHTMLTableRow:String=
		{
			val scorecolor=if(scorenumerical>=0) "#007f00" else "#7f0000"
			s"""
			|<tr>
			|<td><font color="blue">$bestmove</font></td>
			|<td><font color="$scorecolor">$scoreverbal</font></td>
			|<td>$depth</td>
			|<td>$nodesverbal</td>
			|<td>$npsverbal</td>
			|<td><font color="#00007f">$pvreststr</font></td>
			|</tr>
			""".stripMargin
		}
	}

	case class DepthItem(depth:Int=1)
	{
		var maxmultipv=1
		var pvitems=Map[Int,PvItem]()

		def ParseLine(line:String)
		{
			val pvitem=PvItem().ParseLine(line)
			if(pvitem.haspv)
			{
				val multipv=if(pvitem.hasmultipv) pvitem.multipv else { maxmultipv+=1 ; maxmultipv }

				if(!pvitems.contains(multipv)) pvitems+=(multipv->PvItem())

				pvitems+=(multipv->pvitems(multipv).UpdateWith(pvitem))
			}
		}

		def ReportHTML:String=
		{
			val multipvs=pvitems.keys.toList.sorted
			val multipvscontent=(for(multipv<-multipvs) yield pvitems(multipv).ReportHTMLTableRow).mkString("\n")
			s"""
				|<table cellpadding=3 cellspacing=3>
				|<tr>
				|<td>Move</td>
				|<td>Score</td>
				|<td>Depth</td>
				|<td>Nodes</td>
				|<td>Nps</td>
				|<td>Pv</td>
				|</tr>
				|$multipvscontent
				|</table>
			""".stripMargin
		}
	}

	var thinkingoutput=ThinkingOutput()

	case class ThinkingOutput()
	{
		var maxdepth=1
		var depthitems=Map[Int,DepthItem]()

		def UpdateEngineOut(content:String)
		{
			val cwe=Builder.getwebe(s"$path#engineouttext")
			if(cwe==null) return
			cwe.loadContent(content)
		}

		def ParseLine(line:String)
		{
			val pvitem=PvItem().ParseLine(line)
			val depth=if(pvitem.hasdepth) pvitem.depth else maxdepth
			if(depth>maxdepth) maxdepth=depth
			if(!depthitems.contains(depth)) depthitems+=(depth->DepthItem(depth))
			depthitems(depth).ParseLine(line)
			Platform.runLater(new Runnable{def run{
				UpdateEngineOut(ReportHTML)
			}})
		}

		def ReportHTML:String=
		{
			val depths=depthitems.keys.toList.sorted.reverse
			val depthscontent=(for(depth<-depths) yield depthitems(depth).ReportHTML).mkString("<hr>")
			s"""
				|$depthscontent
			""".stripMargin
		}
	}
}

case class GEngineList(var we:WebEngine=null)
{
	var enginelist=Array[GEngine]()

	var multipv=1

	def SetMultipv(set_multipv:Int,g:game)
	{
		multipv=set_multipv
		for(engine<-enginelist) engine.SetMultipv(multipv,g)
	}

	def StartAll(g:game)
	{
		for(engine<-enginelist) engine.Start(g)
		Update
	}

	def StopAll()
	{
		for(engine<-enginelist) engine.Stop
	}

	def CheckRestartAll(g:game)
	{
		for(engine<-enginelist) engine.CheckRestart(g)
	}

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