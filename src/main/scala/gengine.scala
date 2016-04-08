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
import commands._
import move._

import collection.JavaConverters._

import org.apache.commons.lang.time.DurationFormatUtils.formatDuration

////////////////////////////////////////////////////////////////////

case class EngineGames(
	DisableBoardControls:()=>Unit,
	EnableBoardControls:()=>Unit,
	GetEngineList:()=>GEngineList,
	GuiUpdate:()=>Unit
)
{
	var gamerunning=false

	var gamethread:Thread=null

	def Update(content:String)
	{
		Platform.runLater(new Runnable{def run{
			val we=Builder.getwebe(s"enginegamestext")
			if(we==null) return
			we.loadContent(content)
		}})
	}

	var playerwhite:GEngine=null
	var playerblack:GEngine=null

	var selectplayersmessage=""
	def SelectPlayers:Boolean=
	{
		playerwhite=null
		playerblack=null
		for(engine<-GetEngineList().enginelist)
		{
			if(engine.Loaded)
			{
				if(playerwhite==null) playerwhite=engine
				else if(playerblack==null) playerblack=engine
				else return true
			}
		}
		if((playerwhite!=null)&&(playerblack!=null)) return true
		selectplayersmessage="Game could not be started. Please load at least two engines."
		return false
	}

	var isconventional=true
	var isincremental=false
	var initialtime=300000
	var initialtimemin=5
	var movestogoperround=40
	var incrementpermove=0
	var incrementpermovesec=0
	var incrementpermovestogo=300000
	var incrementpermovestogomin=5
	var timecontrolverbal=""
	var movestogo=40
	var level="40 5 0"
	var turn="white"
	var players=Map[String,GEngine]()

	def SecondsVerbal(secs:Int):String=
	{
		val mod=(secs%60)
		val mins=(secs-mod)/60
		if(mod==0)
		{
			return s"$mins min(s)"
		}
		return s"$mins min(s) $mod sec(s)"
	}

	def GetTimeControl
	{
		isconventional=Builder.gcb("timecontrolconventional",true)
		isincremental=Builder.gcb("timecontrolincremental",false)

		initialtimemin=Builder.gci("timecontroltime#selected",5)

		incrementpermovesec=0

		if(isincremental)
		{
			initialtimemin=Builder.gci("timecontrolincrementaltime#selected",2)
			incrementpermovesec=Builder.gci("timecontrolincrementalincrement#selected",3)
		}

		incrementpermove=incrementpermovesec*1000

		initialtime=initialtimemin*60*1000

		incrementpermovestogomin=initialtimemin
		incrementpermovestogo=initialtime

		movestogoperround=Builder.gci("timecontrolnumberofmoves#selected",40)

		if(isincremental) movestogoperround=0

		movestogo=movestogoperround

		level=s"$movestogoperround $incrementpermovestogomin $incrementpermovesec"

		playerwhite.time=initialtime
		playerblack.time=initialtime

		turn="white"

		players+=("white"->playerwhite)
		players+=("black"->playerblack)

		val ipmtgsverbal=SecondsVerbal(incrementpermovestogo/1000)

		timecontrolverbal=s"$movestogoperround move(s) in $ipmtgsverbal"

		if(isincremental)
		{
			timecontrolverbal=s"$initialtimemin min(s) + $incrementpermovesec sec(s)"
		}
	}

	var timestep=100

	var bestmove:String=null

	def StartThinking(engine:GEngine,turn:String,issuego:Boolean)
	{
		engine.wtime=playerwhite.time
		engine.btime=playerblack.time
		engine.winc=incrementpermove
		engine.binc=incrementpermove
		engine.movestogo=movestogo
		engine.fen=commands.g.report_fen
		engine.usermove=bestmove
		engine.issuego=issuego
		if(turn=="white") engine.otim=playerblack.time else engine.otim=playerwhite.time
		engine.StartThinking
	}

	def UpdateGameStatus:String=
	{
		val wtime=formatDuration(playerwhite.time,"HH:mm:ss")
		val btime=formatDuration(playerblack.time,"HH:mm:ss")
		val whitecolor="#00af00"
		val blackcolor="#af0000"
		val timestyle="font-size:36px; font-family: monospace; font-weight: bold; padding: 3px; border-style: solid; border-width: 2px; border-color: #000000; border-radius: 10px;"
		var whitebckg=if(turn=="white") "#afffaf" else "#afafaf"
		var blackbckg=if(turn=="black") "#afffaf" else "#afafaf"
		val sidespan=s"""<span style="padding: 5px; border-style: solid; border-width: 2px; border-color: #000000; border-radius: 10px; font-size: 20px;">"""
		val namew=playerwhite.GetDisplayName
		val nameb=playerblack.GetDisplayName
		val namestyle="font-size: 20px; color: #0000ff;"
		val timecontrolcolor="#0000af"
		val timecontrolinfo=if(isconventional) s"<i><font size=5><b>$movestogo</b></font> move(s) to go</i>" else ""
		s"""
			|<table cellpadding="3" cellspacing="3">
			|<tr>
			|<td>$sidespan<font color="$whitecolor">White</font></span></td>
			|<td><span style="$timestyle background-color: $whitebckg;">$wtime</span></td>
			|<td>$sidespan<font color="$blackcolor">Black</font></span></td>
			|<td><span style="$timestyle background-color: $blackbckg">$btime</span></td>
			|<td><font color="$timecontrolcolor">$timecontrolinfo</td>
			|</tr>
			|</table>
			|<table cellpadding="3" cellspacing="3">
			|<tr>
			|<td>White</td>
			|<td style="$namestyle"><font color="$whitecolor">$namew</font></td>
			|</tr>
			|<tr>
			|<td>Black</td>
			|<td style="$namestyle"><font color="$blackcolor">$nameb</font></td>
			|</tr>
			|<tr>
			|<td>Time control</td>
			|<td style="color: $timecontrolcolor; font-size: 18px; font-weight: bold;" colspan="3"><i>$timecontrolverbal</i></td>
			|</tr>
			|</table>
		""".stripMargin
	}

	def StartGame(fromposition:Boolean=false)
	{
		if(gamerunning) return
		if(!SelectPlayers)
		{
			Update(selectplayersmessage)
			return
		}
		if((!playerwhite.CanSetboard)||(!playerblack.CanSetboard))
		{
			Update("Engine support for starting game from a given position is missing. Game could not be started.")
			return
		}
		if(commands.g.report_result!=null)
		{
			Update("Game starting position is final. Game could not be started.")
			return
		}
		gamethread=new Thread(new Runnable{def run{
			gamerunning=true
			var cnt=0
			var interrupted=false
			bestmove=null
			var issuego=true
			DisableBoardControls()
			GetTimeControl
			playerwhite.SetMultipv(1,commands.g)
			playerblack.SetMultipv(1,commands.g)
			val fen=commands.g.report_fen
			playerwhite.NewGame(level,fromposition,fen)
			playerblack.NewGame(level,fromposition,fen)
			val initturn=turn
			var onturn=players(turn)
			Platform.runLater(new Runnable{def run{
				playerwhite.OpenConsole
				playerblack.OpenConsole
				onturn.ToTop
			}})
			StartThinking(onturn,turn,true)
			var stepcnt=0
			var gameresult:GameResult=null
			while((!Thread.currentThread.isInterrupted())&&(!interrupted)&&(gameresult==null))
			{
				if((stepcnt%5)==0)
				{
					Update(UpdateGameStatus)
				}
				if(onturn.thinking)
				{
					onturn.time-=timestep
					if(onturn.time<=0)
					{
						if(turn=="white")
						{
							gameresult=GameResult(-1,"0-1","white lost on time")
						} else {
							gameresult=GameResult(1,"1-0","black lost on time")
						}
					}
				}
				else
				{
					bestmove=onturn.bestmove
					val true_algeb=commands.g.b.to_true_algeb(bestmove)
					val m=move(fromalgeb=true_algeb)
					commands.g.makeMove(m)
					Platform.runLater(new Runnable{def run{
						GuiUpdate()
					}})
					gameresult=commands.g.report_result
					if(gameresult!=null)
					{

					}
					else
					{
						if(isincremental)
						{
							onturn.time+=incrementpermove
						}
						if(turn=="white") turn="black" else turn="white"
						onturn=players(turn)
						Platform.runLater(new Runnable{def run{
							onturn.ToTop
						}})			
						StartThinking(onturn,turn,issuego)
						issuego=false
						if(turn==initturn)
						{
							if(isconventional)
							{
								movestogo-=1
							}
						}
						if(isconventional)
						{
							if(movestogo==0)
							{
								movestogo=movestogoperround
								playerwhite.time+=incrementpermovestogo
								playerblack.time+=incrementpermovestogo
							}
						}
					}
				}
				if(gameresult==null)
				{
					try{Thread.sleep(timestep)}catch{case e:Throwable=>{interrupted=true}}
					stepcnt+=1
				}
			}
			playerwhite.StopForced
			playerblack.StopForced
			Platform.runLater(new Runnable{def run{
				playerwhite.Reuse
				playerblack.Reuse
			}})
			if(gameresult==null)
			{
				Update(UpdateGameStatus+"<br>Game aborted.")
			} else {
				var resultverbal=gameresult.resultstr+" ( "+gameresult.resultreason+" )"
				Update(UpdateGameStatus+s"<br>Game finished. Result: $resultverbal")
			}
			EnableBoardControls()
			gamerunning=false
		}})

		gamethread.start()
	}

	def AbortGame
	{
		if(gamethread==null) return
		if(gamerunning)
		{
			gamethread.interrupt()
		}
	}

	def ShutDown
	{
		AbortGame
	}
}

case class GEngine(
	var id:Int=0,
	val enginedata:Data=null,
	val set_handler:Builder.THandler=null
)
{
	var path:String=""
	var pathid:String=""
	var commandline:String=""
	var protocol:String="UCI"

	val protocols=List("UCI","XBOARD")

	var engineprocess:Process=null
	var enginein:InputStream=null
	var engineout:OutputStream=null
	var enginereadthread:Thread=null

	var autoload=false

	var xboardstate="Observing"

	val globalhandler=set_handler

	FromData(enginedata)

	var multipv=1

	var time=300000

	var otim=300000

	var wtime=300000

	var btime=300000

	var winc=0

	var binc=0

	var movestogo=40

	var usermove:String=null

	var issuego=true

	var thinking=false

	var fen=""

	def Loaded:Boolean=(engineprocess!=null)

	def CanSetboard:Boolean=
	{
		if(protocol=="UCI") return true

		if(protocol=="XBOARD")
		{
			if(features.setboard) return true
			return false
		}

		return false
	}

	def Reload
	{
		if(!Loaded) return
		Unload
		Load
		OpenConsole
	}

	def Reuse
	{
		if(protocol=="XBOARD")
		{
			if(!features.reuse)
			{
				Reload
			}
		}
	}

	def IssueVariantXBOARD
	{
		if(settings.getvariant=="Atomic")
		{
			IssueCommand("variant atomic")
		}
	}

	def NewGame(level:String="40 5 0",fromposition:Boolean=false,fen:String="")
	{
		usermove=null

		if(protocol=="UCI")
		{
			if(fromposition)
			{
				IssueCommand(s"position fen "+fen)
			}
			else
			{
				IssueCommand(s"position startpos")
			}
		}

		if(protocol=="XBOARD")
		{
			IssueCommand("new")
			IssueCommand("random")
			IssueVariantXBOARD
			IssueCommand(s"level $level")
			IssueCommand("post")
			IssueCommand("force")
			if(fromposition)
			{
				IssueCommand(s"setboard "+fen)
			}
		}
	}

	def StartThinking
	{

		thinkingoutput=ThinkingOutput()

		thinking=true

		if(protocol=="UCI")
		{
			IssueCommand(s"position fen $fen")
			val issuemovestogo=if(movestogo!=0) s" movestogo $movestogo" else " movestogo 5"
			IssueCommand(s"go wtime $wtime btime $btime winc $winc binc $binc"+issuemovestogo)
		}

		def IssueXBOARDStart
		{
			if(usermove!=null)
			{
				IssueCommand(s"usermove $usermove")
				if(issuego)
				{
					IssueCommand("go")
				}
			}
			else
			{
				IssueCommand("go")
			}
		}

		def IssueXBOARDTimeControl
		{
			val centitime:Int=(time/10).toInt
			val centiotim:Int=(otim/10).toInt
			IssueCommand(s"time $centitime")
			IssueCommand(s"otim $centiotim")
		}

		if(protocol=="XBOARD")
		{			
			if(features.colors)
			{
				IssueXBOARDStart
				IssueXBOARDTimeControl
			} else {
				IssueXBOARDTimeControl
				IssueXBOARDStart
			}
		}		

	}

	def SetPath(p:String)
	{
		path=p
		CreatePathId
	}

	def SetCommandLine(cv:String)
	{
		val wasloaded=Loaded
		Unload
		commandline=cv
		CreatePathId
		if(wasloaded) Load
	}

	def SetMultipv(set_multipv:Int,g:game)
	{
		if(engineprocess==null) return

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
			if(ev.id==s"$pathid#issueenginecommand")
			{
				IssueConsoleEngineCommand
			}
		}

		if(ev.kind=="textfield entered")
		{
			if(ev.id==s"$pathid#enginecommand")
			{
				IssueConsoleEngineCommand
			}
		}
	}

	def IssueConsoleEngineCommand
	{
		val etext=Builder.gettext(s"$pathid#enginecommand")
		val command=etext.getText
		etext.setText("")
		IssueCommand(command)
	}

	def CloseConsole
	{
		if(Builder.stages.contains(pathid))
		{
			Builder.closeStage(pathid)
		}
	}

	def ToTop
	{
		if(Builder.stages.contains(pathid))
		{
			Builder.stages(pathid).ToTop
		}	
	}

	def OpenConsole
	{
		if(engineprocess==null)
		{
			CloseConsole
			return
		}

		if(Builder.stages.contains(pathid))
		{
			Builder.stages(pathid).ToTop
			return
		}	

		val blob=s"""
			|<scrollpane>
			|<tabpane>
			|<tab caption="Search output">
			|<scrollpane id="engineoutscrollpane" width="800">
			|<webview id="$pathid#engineouttext" height="3000" width="3000"/>
			|</scrollpane>
			|</tab>
			|<tab caption="Console">
			|<vbox padding="5" gap="5">
			|<hbox padding="5" gap="5">
			|<textfield style="-fx-font-size: 18px; -fx-text-fill: #00007f;" id="$pathid#enginecommand"/>
			|<button id="$pathid#issueenginecommand" text="Issue" style="round"/>
			|</hbox>
			|<scrollpane id="engineconsolescrollpane" width="800">
			|<webview id="$pathid#engineconsoletext" height="3000" width="3000"/>
			|</scrollpane>
			|</vbox>
			|</tab>
			|<tab caption="Settings">
			|<scrollpane id="enginesettingsscrollpane" width="800">
			|<vbox id="$pathid#enginesettingsvbox" height="3000" width="3000"/>
			|</scrollpane>
			|</tab>
			|</tabpane>
			|</scrollpane>
		""".stripMargin
		Builder.MyStage(pathid,modal=false,set_handler=handler,title=GetDisplayName+" console",blob=blob)
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
		if(Builder.stages.contains(pathid))
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
		println(s"engine $pathid unloaded")
	}

	case class Option(
		var name:String="",
		var kind:String="",
		var minstr:String="",
		var maxstr:String="",
		var defaultstr:String="",
		var send:Boolean=true,
		var subkind:String=""
	)
	{
		def Apply
		{
			if((kind!="button")&&(send==true))
			{
				val id=s"engineoptions#$pathid#$name"

				var value=Builder.getcvevals(id,defaultstr)

				if((kind=="spin")&&(value!=defaultstr)) value=""+value.toDouble.toInt

				if(protocol=="UCI")
				{
					IssueCommand("setoption name "+name+" value "+value)
				}

				if(protocol=="XBOARD")
				{
					var xboardvalue=value
					if(kind=="check")
					{
						xboardvalue=if(value=="true") "1" else "0"
					}

					IssueCommand("option "+name+"="+xboardvalue)
				}
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
			val id=s"engineoptions#$pathid#$name"
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

	case class Id(
		var name:String="",
		var author:String=""
	)
	{
		def ParseLine(line:String)
		{
			val tokenizer=Tokenizer(line)
			val head=tokenizer.Get
			if(head!=null)
			{
				if(protocol=="UCI")
				{
					if(head=="id")
					{
						val token=tokenizer.Get
						val value=tokenizer.GetRest
						if(value==null) return
						if(token=="name") name=value
						if(token=="author") author=value
					}
				}
			}
		}
	}

	case class Options(var options:List[Option]=List[Option]())
	{

		InitOptions

		def InitOptions
		{
			Add(Option(name="Auto set FEN",kind="check",defaultstr="true",send=false))
			Add(Option(name="Command after set FEN",kind="string",defaultstr="",send=false))
			Add(Option(name="Auto start",kind="check",defaultstr="true",send=false))
		}

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
				if(protocol=="UCI")
				{
					IssueCommand("setoption name "+truename+" value "+value)
				}
				if(protocol=="XBOARD")
				{
					IssueCommand("option "+truename+"="+value)
				}
			} else {
				if(protocol=="UCI")
				{
					IssueCommand("setoption name "+buttonname+" value ")
				}
				if(protocol=="XBOARD")
				{
					IssueCommand("option "+buttonname)
				}
			}
		}

		if(ev.kind=="slider changed")
		{
			Builder.setcveval(ev.id,StringData(ev.value))

			val slidername=GetNameFromId(ev.id)

			val sliderint=ev.value.toDouble.toInt

			if(protocol=="UCI")
			{
				IssueCommand("setoption name "+slidername+" value "+sliderint)
			}
			if(protocol=="XBOARD")
			{
				IssueCommand("option "+slidername+"="+sliderint)
			}
		}

		if(ev.kind=="checkbox changed")
		{
			Builder.setcveval(ev.id,StringData(ev.value))

			val checkboxname=GetNameFromId(ev.id)

			val checkboxbool=ev.value

			if(protocol=="UCI")
			{
				IssueCommand("setoption name "+checkboxname+" value "+checkboxbool)
			}
			if(protocol=="XBOARD")
			{
				val xboardcheckboxbool=if(checkboxbool=="true") "1" else "0"
				IssueCommand("option "+checkboxname+"="+xboardcheckboxbool)
			}
		}
	}

	def BuildOptions
	{
		val svboxcomp=Builder.getcomp(s"$pathid#enginesettingsvbox")
		if(svboxcomp==null) return
		val svbox=svboxcomp.node.asInstanceOf[VBox]
		if(svbox==null) return
		val optionscontent=options.ReportXML
		val blob=s"""
			|<vbox padding="3" gap="3">
			|$optionscontent
			|</vbox>
		""".stripMargin
		val scenegraph=Builder.build(s"$pathid#enginesettingsvboxscenegraph",options_handler,blob=blob)
		svbox.getChildren().clear()
		svbox.getChildren().add(scenegraph)
	}

	def ParseXBOARDBool(str:String,default:Boolean):Boolean=
	{
		if(str==null) return default
		if(str=="1") return true
		if(str=="0") return false
		return default
	}

	case class Features(
		var setboard:Boolean=false,
		var analyze:Boolean=true,
		var colors:Boolean=true,
		var done:Boolean=false,
		var reuse:Boolean=true,
		var myname:String=""
	)
	{
		def ParseLine(line:String)
		{
			val tokenizer=Tokenizer(line)
			val head=tokenizer.Get
			if(head==null) return
			if(head!="feature") return
			while(tokenizer.HasToken)
			{
				val token=tokenizer.Get
				val parts=token.split("=").toList
				if(parts.length==2)
				{
					val feature=parts(0)
					var value=parts(1)

					if(value.length>0)
					{
						var needjoin=false
						var joinparts=List[String]()
						if(value(0)=='"')
						{
							if(value.length>1)
							{
								if(value(value.length-1)=='"')
								{
									value=value.substring(1,value.length-1)
								} else {
									joinparts=List[String](value.substring(1))
									needjoin=true
								}
							} else {
								joinparts=List[String]("")
								needjoin=true
							}
							var closed=false
							if(needjoin)
							{
								while(tokenizer.HasToken&&(!closed))
								{
									var part=tokenizer.Get
									if(part.length>0)
									{
										if(part(part.length-1)=='"')
										{
											part=part.substring(0,part.length-1)
											closed=true
										}
									}
									joinparts=joinparts:+part
								}
								value=joinparts.mkString(" ")
							}
						}
					}

					if(feature=="myname") myname=value
					if((feature=="setboard")&&(value=="1")) setboard=true
					if((feature=="analyze")&&(value=="0")) analyze=false
					if((feature=="colors")&&(value=="0")) colors=false
					if((feature=="reuse")&&(value=="0")) reuse=false
					if((feature=="done")&&(value=="1")) done=true

					if(feature=="option")
					{
						val vtokenizer=Tokenizer(value)
						var nameparts=List[String]()
						var nameend=false
						while(vtokenizer.HasToken&&(!nameend))
						{
							val token=vtokenizer.Poll
							if(token.length>0)
							{
								if(token(0)=='-')
								{
									nameend=true
								} else {
									vtokenizer.Get
									nameparts=nameparts:+token
								}
							}
						}
						var kind=vtokenizer.Get
						if(kind!=null)
						{
							if(kind.length>0)
							{
								val name=nameparts.mkString(" ")
								kind=kind.substring(1)

								if(kind=="check")
								{
									val checkdefaulttoken=vtokenizer.Get
									val checkdefaultstr=""+ParseXBOARDBool(checkdefaulttoken,false)									
									val option=Option(kind=kind,name=name,defaultstr=checkdefaultstr)
									options.Add(option)
								} else if((kind=="spin")||(kind=="slider")) {
									val spindefaulttoken=vtokenizer.Get
									val spindefaultstr=""+ParseInt(spindefaulttoken,1)
									val spinmintoken=vtokenizer.Get
									val spinminstr=""+ParseInt(spinmintoken,0)
									val spinmaxtoken=vtokenizer.Get
									val spinmaxstr=""+ParseInt(spinmaxtoken,0)
									val option=Option(kind="spin",name=name,
										defaultstr=spindefaultstr,minstr=spinminstr,maxstr=spinmaxstr,subkind=kind)
									options.Add(option)
								} else if((kind=="button")||(kind=="save")||(kind=="reset")) {
									val option=Option(kind="button",name=name,subkind=kind)
									options.Add(option)
								} else if((kind=="string")||(kind=="file")||(kind=="path")) {
									val stringdefaulttoken=vtokenizer.GetRest
									val stringdefaultstr=if(stringdefaulttoken==null) "" else stringdefaulttoken
									val option=Option(kind="string",name=name,defaultstr=stringdefaultstr,subkind=kind)
									options.Add(option)
								} else if(kind=="combo") {
									// not implemented
								} else {
									// unknown option
								}
							}
						}
					}
				}
			}
		}
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
				uciid.ParseLine(line)
			}
		}

		if(protocol=="XBOARD")
		{
			features.ParseLine(line)
			if(features.done)
			{
				startup=false
				options.ApplyAll
			}
		}
	}

	var bestmove:String=null

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

		var tokenizer=Tokenizer(line)

		if(protocol=="UCI")
		{
			val token=tokenizer.Get
			if(token=="bestmove")
			{
				bestmovereceived=true
				bestmove=tokenizer.Get
				thinking=false
			}
		}

		if(protocol=="XBOARD")
		{
			val token=tokenizer.Get
			if(token=="move")
			{
				bestmovereceived=true
				bestmove=tokenizer.Get
				thinking=false
			}
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
		val cwe=Builder.getwebe(s"$pathid#engineconsoletext")
		if(cwe==null) return
		cwe.loadContent(content)
	}

	case class Log(buffersize:Int=1000)
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

	def UpdateXBOARDState(command:String)
	{
		if(command=="analyze")
		{
			xboardstate="Analyzing"
		}
		if(command=="force")
		{
			xboardstate="Observing"
		}
	}

	def IssueCommand(command:String)
	{
		if(command==null) return
		val fullcommand=command+"\n"
		try
		{
			engineout.write(fullcommand.getBytes())
			engineout.flush()
			UpdateXBOARDState(command)
			log.Add(LogItem(command,"in"))
		}
		catch
		{
			case e: Throwable =>
			{
				println(s"engine write IO exception, command: $command, id: $id, pathid: $pathid")
				e.printStackTrace
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
						println(s"engine read not a char exception, chunk: $chunkobj, id: $id, pathid: $pathid")
					}
				}
			}
			catch
			{
				case e: Throwable =>
				{
					println(s"engine read IO exception, id: $id, pathid: $pathid")
				}
			}
		}
	}})}

	var options=Options()
	var features=Features()
	var uciid=Id()

	def GetDisplayName:String=
	{
		if(protocol=="UCI")
		{
			if(uciid.name!="")
			{
				if(uciid.author!="")
				{
					return uciid.name+" by "+uciid.author
				} else {
					return uciid.name
				}
			}
		}
		if(protocol=="XBOARD")
		{
			if(features.myname!="") return features.myname
		}
		ParseEngineNameFromPath(path)
	}

	def ProtocolStartup
	{
		startup=true

		options=Options()
		features=Features()
		uciid=Id()

		if(protocol=="UCI")
		{
			IssueCommand("uci")
		}

		if(protocol=="XBOARD")
		{
			startup=false
			IssueCommand("xboard")
			IssueCommand("protover 2")
			startup=true
		}

		var cnt=0
		while((startup)&&(cnt< 20))
		{
			try{Thread.sleep(100)}catch{case e:Throwable=>{}}
			cnt+=1
		}

		startup=false
	}

	def Load:Boolean=
	{
		Unload
		val progandargs:List[String]=path+:commandline.split(" ").toList
		val processbuilder=new ProcessBuilder(progandargs.asJava)
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
        println(s"engine $pathid loaded")
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

			val commandlinedata=Data.get(enginemapdata,"commandline")

			if(commandlinedata != null)
			{
				if(commandlinedata.isInstanceOf[StringData])
				{
					commandline=commandlinedata.asInstanceOf[StringData].value					
				}
			}

			CreatePathId
		}
	}

	def CreatePathId
	{
		pathid=path+" "+commandline
	}

	def ToData:Data=
	{
		var mapdata=Map[String,Data]()
		mapdata+=("path" -> StringData(path))
		mapdata+=("protocol" -> StringData(protocol))
		mapdata+=("autoload" -> StringData(""+autoload))
		mapdata+=("commandline" -> StringData(commandline))
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
		val consoleopen=Builder.stages.contains(pathid)
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
			|<table>
			|<tr>
			|<td class="italiclabel">command line</td>
			|<td><input type="text" id="commandline$id" value="$commandline"></td>
			|<td><input type="button" value="Apply command line" onclick="idstr='$id'; command='applycommandline';"></td>
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

	def GetOption(name:String,default:String=""):String=
	{
		val id=s"engineoptions#$pathid#$name"

		var value=Builder.getcvevals(id,default)

		value
	}

	def GetBoolOption(name:String,default:Boolean):Boolean=
	{
		val boolstr=GetOption(name,""+default)

		if(boolstr=="true") return true

		false
	}

	def XBOARDIssueExitOrForce
	{
		if(xboardstate=="Analyzing")
		{
			IssueCommand("exit")
		} else {
			IssueCommand("force")
		}				
	}

	def Start(g:game)
	{
		if(engineprocess==null) return
		if(startup) return
		if(running) return
		thinkingoutput=ThinkingOutput()
		OpenConsole
		val autosetfen=GetBoolOption("Auto set FEN",true)
		val commandaftersetfen=GetOption("Command after set FEN","")
		val autostart=GetBoolOption("Auto start",true)
		def IssueCommandAfterSetFEN
		{
			if(commandaftersetfen!="") IssueCommand(commandaftersetfen)
		}
		if(protocol=="XBOARD")
		{
			val truealgebline=g.current_line_true_algeb_moves
			if(truealgebline.length==0) return

			if(autosetfen)
			{
				val infinitethinkingtime=24*60*1000
				XBOARDIssueExitOrForce
				IssueCommand("new")
				IssueCommand("force")
				IssueCommand("post")
				IssueVariantXBOARD
				IssueCommand("analyze")
				for(truealgeb<-truealgebline)
				{
					IssueCommand(s"usermove $truealgeb")
				}
				running=true
			}
		}
		if(protocol=="UCI")
		{
			val fen=g.report_fen
			if(IsAtomkraft)
			{
				if(autosetfen)
				{
					val content = new ClipboardContent()
	                content.putString(fen)
	                clip.setContent(content)
            	}

            	IssueCommandAfterSetFEN
                
                if(autostart)
                {
                	IssueCommand("1\n4")

                	running=true
            	}
			}
			else
			{
				if(autosetfen)
				{
					IssueCommand("position fen "+fen)
				}

				IssueCommandAfterSetFEN

				if(autostart)
				{
					IssueCommand("go infinite")

					running=true
				}
			}
		}
	}

	var bestmovereceived=false

	def Stop
	{
		StopInner()
	}

	def StopForced
	{
		StopInner(forced=true)
	}

	def StopInner(forced:Boolean=false)
	{
		if(engineprocess==null) return
		if(startup) return
		if(!forced)
		{
			if(!running) return
		}
		if(protocol=="XBOARD")
		{
			XBOARDIssueExitOrForce
			try{Thread.sleep(200)}catch{case e:Throwable=>{}}
			running=false
		}
		if(protocol=="UCI")
		{
			bestmovereceived=false
			if(IsAtomkraft)
			{
				IssueCommand("s")
			} else {
				IssueCommand("stop")
			}
			if(!forced)
			{
				while(!bestmovereceived)
				{
					try{Thread.sleep(50)}catch{case e:Throwable=>{}}
				}
			}
			running=false
		}
	}

	def CheckRestart(g:game)
	{
		if(engineprocess==null) return
		if(running&&(GetBoolOption("Auto start",true)))
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

	def IsInt(str:String):Boolean=
	{
		if(str==null) return false
		try
		{
			val intvalue=str.toInt
			return true
		}
		catch{case e: Throwable => {}}
		return false
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
		def FormatNodes(nodes:Int,unit:Int):String=
		{
			"%.2f".format(nodes.toDouble/unit.toDouble)
		}
		def GetNodesVerbal(nodes:Int):String=
		{
			if(nodes< 1000) return ""+nodes else
			if(nodes< 1000000) return ""+FormatNodes(nodes,1000)+" kN" else return FormatNodes(nodes,1000000)+" MN"
		}
		def GetNpsVerbal(nodes:Int):String=
		{
			if(nps< 1000) return ""+nps else
			if(nps< 1000000) return ""+FormatNodes(nps,1000)+" kN/s" else return FormatNodes(nps,1000000)+" MN/s"
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
					if(name=="nodes")
					{
						nodes=ParseInt(tokenizer.Get,nodes)
						nodesverbal=GetNodesVerbal(nodes)
						hasnodes=true
					}
					if(name=="nps")
					{
						nps=ParseInt(tokenizer.Get,nps)
						npsverbal=GetNodesVerbal(nps)
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
			if(protocol=="XBOARD")
			{
				val parts=line.split(" ").toList
				val len=parts.length
				if(len< 5) return this
				if(IsInt(parts(0))&&IsInt(parts(1))&&IsInt(parts(2))&&IsInt(parts(3)))
				{
					depth=ParseInt(parts(0),depth)
					hasdepth=true

					scorenumerical=ParseInt(parts(1),depth)
					scoreverbal=if(scorenumerical>0) "+"+scorenumerical else ""+scorenumerical
					hasscore=true

					time=ParseInt(parts(2),time)*10
					hastime=true

					nodes=ParseInt(parts(3),nodes)
					nodesverbal=GetNodesVerbal(nodes)
					hasnodes=true

					tokenizer.Get
					tokenizer.Get
					tokenizer.Get
					tokenizer.Get
					pv=tokenizer.GetRest
					haspv=true
					val pvtokens=Tokens(pv)
					bestmove=pvtokens.head
					pvrest=pvtokens.tail
					pvreststr=pvrest.mkString(" ")
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
			val timeformatted=formatDuration(time,"HH:mm:ss")
			s"""
			|<tr>
			|<td><font color="blue">$bestmove</font></td>
			|<td><font color="$scorecolor">$scoreverbal</font></td>
			|<td>$depth</td>
			|<td>$timeformatted</td>
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
				|<td>Time</td>
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
			val cwe=Builder.getwebe(s"$pathid#engineouttext")
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

			enginelist(id).SetPath(path)

		}
	}

	def Del(id:Int)
	{
		val engine=enginelist(id)
		engine.Unload
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

		if(command=="applycommandline")
		{
			val cv=we.executeScript(s"document.getElementById('commandline$idstr').value").toString()
			enginelist(idstr.toInt).SetCommandLine(cv)
			Update
		}

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
			|<input type="button" value="Add new engine" onclick="command='add';""><br>
			|$listhtml
			|</body>
			|</html>
		""".stripMargin
	}
}

////////////////////////////////////////////////////////////////////