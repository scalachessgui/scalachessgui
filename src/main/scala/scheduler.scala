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
import piece._

import collection.JavaConverters._

import org.apache.commons.lang.time.DurationFormatUtils.formatDuration
import org.apache.commons.lang.time.DateFormatUtils._
import org.apache.commons.io.FileUtils._

import java.util.Date

////////////////////////////////////////////////////////////////////

object SchedulerGlobals
{
	val whitetextcolor="#007f00"
	val blacktextcolor="#7f0000"
	var elapsed=0
	var running=false

	def elapsedf:String=formatDuration(elapsed,"HH:mm:ss")
}

// scheduler is used to create a schedule for playing engine games
// a schedule is created from:
// 1) selected engines: engines that are selected for playing games
// 2) setup: starting conditions of the games to be played
// the created schedule is a list of games to be played
// it is executed by the scheduler playing one by one the scheduled games
// the execution can be paused and resumed

// EngineInfo stores information on the selected state of an engine
case class EngineInfo(
	var name:String="",
	var selected:Boolean=false,
	var pathid:String=""
)
{
	def SetSelected(value:String)
	{
		selected=if(value=="true") true else false
	}

	def SetSelected(value:Boolean)
	{
		selected=value
	}

	def ToData:Data=
	{
		MapData(map=Map[String,Data]("name"->StringData(name),"selected"->StringData(""+selected)))
	}

	def reportXML:String=
	{
		s"""
			|<hbox padding="5" gap="5">
			|<checkbox id="scheduler#selectedengines#$pathid#selected" usevariantentry="true" />
			|<label text="$name" />
			|</hbox>
		""".stripMargin
	}
}

// SelectedEngines keeps track of engines selected for playing games
case class SelectedEngines(
	enginelist:GEngineList
)
{
	var items=Map[String,EngineInfo]()

	def ToData:Data=
	{
		MapData(map=for((k,v)<-items) yield (k->v.ToData))
	}

	// Query opens a modal dialog in which the user can select and deselect engines
	def Query
	{
		def handler(ev:MyEvent)
		{
			if(ev.kind=="checkbox changed")
			{
				var pathid=ev.id
				pathid=pathid.replaceAll("^scheduler#selectedengines#","")
				pathid=pathid.replaceAll("#selected$","")				
				items(pathid).SetSelected(ev.value)				
			}
		}
		val itemscontent=(for((k,v)<-items) yield v.reportXML).mkString("\n")
		val blob=s"""
			|<vbox padding="5" gap="5" width="400">
			|$itemscontent
			|</vbox>
		""".stripMargin
		Builder.MyStage("engineselectdialog",modal=true,set_handler=handler,do_size=false,title="Select engines",blob=blob)
	}

	// synchronize existing engines with selection information
	def Synchronize
	{
		// collect existing engines
		var existingengines=Map[String,GEngine]()
		for(engine<-enginelist.enginelist)
		{
			existingengines+=(engine.pathid->engine)
		}
		// remove engines that are no longer existing
		for((k,v)<-items) if(!existingengines.contains(k)) items-=k
		// add EngineInfo for engines that do not already have one
		for((k,v)<-existingengines) if(!items.contains(k)) items+=(k->
			EngineInfo(v.GetDisplayName(includeauthor=false),false,k))

		for((k,v)<-items) v.SetSelected(Builder.gcveb("scheduler#selectedengines#"+v.pathid+"#selected",false))
	}

	def Save
	{
		val data=ToData
		Builder.setcveval("scheduler#selectedengines",data)
	}

	def Load
	{
		Synchronize
	}

	def ShutDown
	{
		Save
	}
}

// ScheduleSetupItem records information on a single game starting condition
case class ScheduleSetupItem(
	var fen:String="",
	var thematicmoves:String=""
)
{
	val dummy=new game
	dummy.reset
	fen=dummy.report_fen

	def ToData:Data=
	{
		MapData(map=Map[String,Data](
			"fen"->StringData(fen),
			"thematicmoves"->StringData(thematicmoves)
		))
	}

	def ReportHTMLTableRow(i:Int,l:Int):String=
	{
		var deletebutton=if((i==0)&&(l>1)) """<input type="button" value="Delete" onclick="command='delete';">""" else ""
		s"""
			|<tr>
			|<td>FEN</td>
			|<td><font color="#7f0000"><small>$fen</small></font></td>
			|<td>$deletebutton</td>
			|</tr>
			|<tr>
			|<td>Moves</td>
			|<td align="center"><font color="#00007f"><b>$thematicmoves</b></font></td>
			|<td><input type="button" value="Import" onclick="command='import'; param='$i';"></td>
			|</tr>
			|<tr>
			|<td></td>
			|<td><hr></td>
			|<td></td>
			|</tr>
		""".stripMargin
	}

	def FromData(data:Data):ScheduleSetupItem=
	{
		val map=data.asInstanceOf[MapData].map
		if(map.contains("fen"))
		{
			fen=map("fen").asInstanceOf[StringData].value
		}
		if(map.contains("thematicmoves"))
		{
			thematicmoves=map("thematicmoves").asInstanceOf[StringData].value
		}		
		this
	}
}

// ScheduleSetup records the starting conditions from which games should be played
case class ScheduleSetup(
	var items:List[ScheduleSetupItem]=List[ScheduleSetupItem]()
)
{
	def HasItems:Boolean=( items.length > 0 )

	def ToData:Data=
	{
		SeqData(seq=for(item<-items) yield (item.ToData))
	}

	def Clear
	{
		items=List[ScheduleSetupItem]()		
	}

	def Add(item:ScheduleSetupItem)
	{
		items=item+:items
	}

	def CreateStage
	{
		def handler(ev:MyEvent)
		{		
			if(ev.kind=="webview clicked")
			{
				if(ev.id=="schedulesetuptext")
				{
					val we=Builder.getwebe("schedulesetuptext")
					val command=we.executeScript("command").toString()
					val param=we.executeScript("param").toString()
					
					if(command=="delete")
					{
						items=items.tail
						UpdateScheduleSetup
					}

					if(command=="add")
					{
						Add(ScheduleSetupItem())
						UpdateScheduleSetup
					}

					if(command=="import")
					{
						val i=param.toInt
						items(i).fen=commands.g.root.fen
						items(i).thematicmoves=commands.g.current_line_pgn
						UpdateScheduleSetup
					}
				}
			}
		}		
		val blob=s"""			
			|<scrollpane id="schedulesetupscrollpane" width="600.0" height="645.0">
			|<webview id="schedulesetuptext" height="3000.0" width="590.0"/>
			|</scrollpane>			
		""".stripMargin
		Builder.MyStage("schedulesetupstage",modal=false,do_size=false,set_handler=handler,title="Schedule setup",blob=blob)
	}

	def IsStageOpen:Boolean=Builder.stages.contains("schedulesetupstage")

	def ToTop
	{
		if(IsStageOpen)
		{
			Builder.stages("schedulesetupstage").ToTop
		}	
	}

	def ReportHTML:String=
	{
		var i= -1
		val setupitemscontent=(for(item<-items) yield { i+=1; item.ReportHTMLTableRow(i,items.length) }).mkString("\n")
		s"""
			|<script>
			|var command='';
			|var param='';
			|</script>
			|<table cellpadding="3" cellspacing="3">
			|<tr>
			|<td></td>
			|<td><input type="button" value="Add new setup" onclick="command='add';"></td>
			|<td></td>
			|</tr>
			|$setupitemscontent
			|</table>
		""".stripMargin
	}

	def UpdateScheduleSetup
	{		
		Builder.setweb("schedulesetuptext",ReportHTML)
	}

	def ShowScheduleSetup
	{
		
		if(!IsStageOpen) CreateStage
		ToTop
		UpdateScheduleSetup
	}

	def Save
	{
		val data=ToData
		Builder.setcveval("scheduler#schedulesetup",data)
	}

	def Load
	{
		val seqdataobj=Builder.getcveval("scheduler#schedulesetup")
		if(seqdataobj!=null)
		{
			if(seqdataobj.isInstanceOf[SeqData])
			{				
				val itemsdata=seqdataobj.asInstanceOf[SeqData].seq.toList
				items=for(itemdata<-itemsdata) yield ScheduleSetupItem().FromData(itemdata)
			}
		}
		if(items.length<=0)
		{
			// need at least one item
			Add(ScheduleSetupItem())
		}
	}

	def ShutDown
	{
		Save
	}
}

// ScheduleItem holds information on a single game that has to be played
case class ScheduleItem(
	var playerwhitepathid:String="",
	var playerwhitename:String="",
	var playerblackpathid:String="",
	var playerblackname:String="",
	var fen:String="",
	var thematicmoves:String="",
	var result:String="*"
)
{
	def ToData:Data=
	{
		MapData(map=Map[String,Data](
			"playerwhitepathid"->StringData(playerwhitepathid),
			"playerwhitename"->StringData(playerwhitename),
			"playerblackpathid"->StringData(playerblackpathid),
			"playerblackname"->StringData(playerblackname),
			"fen"->StringData(fen),
			"thematicmoves"->StringData(thematicmoves),
			"result"->StringData(result)
		))
	}

	def FromData(data:Data):ScheduleItem=
	{
		val map=data.asInstanceOf[MapData].map
		if(map.contains("playerwhitepathid"))
		{
			playerwhitepathid=map("playerwhitepathid").asInstanceOf[StringData].value
		}
		if(map.contains("playerwhitename"))
		{
			playerwhitename=map("playerwhitename").asInstanceOf[StringData].value
		}
		if(map.contains("playerblackpathid"))
		{
			playerblackpathid=map("playerblackpathid").asInstanceOf[StringData].value
		}
		if(map.contains("playerblackname"))
		{
			playerblackname=map("playerblackname").asInstanceOf[StringData].value
		}
		if(map.contains("fen"))
		{
			fen=map("fen").asInstanceOf[StringData].value
		}
		if(map.contains("thematicmoves"))
		{
			thematicmoves=map("thematicmoves").asInstanceOf[StringData].value
		}
		if(map.contains("result"))
		{
			result=map("result").asInstanceOf[StringData].value
		}
		this
	}

	def SetResult(set_result:String)
	{
		result=set_result
	}

	def ReportHTMLTableRow(current:Boolean):String=
	{
		val whitetextcolor=SchedulerGlobals.whitetextcolor
		val blacktextcolor=SchedulerGlobals.blacktextcolor
		var style=if(current) "border-style: dotted; border-width: 2px; border-radius: 10px; background-color: #afffaf;" else ""
		var movelist=thematicmoves
		s"""
			|<tr style="$style">
			|<td><font color="$whitetextcolor">$playerwhitename</font></td>
			|<td align="center">-</td>
			|<td><font color="$blacktextcolor">$playerblackname</font></td>
			|<td align="center"><font color="#00007f">$result</font></td>
			|<td><small>$movelist</small></td>
			|</tr>
		""".stripMargin
	}
}

// Schedule is a list of games that has to be played
case class Schedule(
	var items:List[ScheduleItem]=List[ScheduleItem](),
	// ptr points to the game that has to be played
	var ptr:Int=0
)
{

	def Done:Boolean=(ptr >= items.length)

	def Current:ScheduleItem=(if(Done) null else items(ptr))

	def ToData:Data=
	{
		val itemsdata=SeqData(seq=for(item<-items) yield (item.ToData))
		MapData(map=Map[String,Data]("items"->itemsdata,"ptr"->StringData(""+ptr)))
	}

	def Save
	{
		val data=ToData
		Builder.setcveval("scheduler#schedule",data)
	}

	def Load
	{
		val mapdataobj=Builder.getcveval("scheduler#schedule")
		if(mapdataobj!=null)
		{
			if(mapdataobj.isInstanceOf[MapData])
			{				
				val map=mapdataobj.asInstanceOf[MapData].map
				if(map.contains("items"))
				{					
					val itemsdata=map("items").asInstanceOf[SeqData].seq.toList
					items=for(itemdata<-itemsdata) yield ScheduleItem().FromData(itemdata)
					if(map.contains("ptr"))
					{
						ptr=map("ptr").asInstanceOf[StringData].value.toInt
					}					
				}
			}
		}
	}

	def Advance
	{
		if(!Done) ptr+=1
	}

	def ReportScheduleHTML(set_from:Int,set_to:Int,reverse:Boolean=false):String=
	{
		var pairings=List[String]()

		var from=set_from
		if(from< 0) from=0
		var to=set_to
		if(to>items.length-1) to=items.length-1
		if((items.length>0)&&(to>=from)) for(i<- from to to)
		{
			pairings=pairings:+items(i).ReportHTMLTableRow(current=(i==ptr))
		}
		if(reverse) pairings=pairings.reverse
		val pairingscontent=pairings.mkString("\n")
		val running=SchedulerGlobals.running
		val timef=SchedulerGlobals.elapsedf
		val status=if(running) s"Running $timef" else "Stopped"
		val statusbutton=if(running)
			"""<input type="button" value="Stop" onclick="command='stop';">"""
			else if(Done)
			"""<input type="button" value="Create" onclick="command='create';">"""
			else
			"""<input type="button" value="Start" onclick="command='start';">"""
		s"""
			|<script>
			|var command='';
			|</script>
			|<table cellpadding="5" cellspacing="5">
			|<tr>
			|<td>$status</td>
			|<td>$statusbutton</td>
			|</tr>
			|</table>
			|<table cellpadding="5" cellspacing="5" style="border-collapse: collapse;">
			|<tr>
			|<td>White</td>
			|<td></td>
			|<td>Black</td>
			|<td>Result</td>
			|</tr>
			|$pairingscontent
			|</table>
		""".stripMargin
	}

	def ReportResultsHTML:String=
	{
		ReportScheduleHTML(0,ptr-1,true)
	}

	def ReportStandingsHTML:String=
	{
		s"""
			|
		""".stripMargin
	}

	def Clear
	{
		items=List[ScheduleItem]()
		ptr=0
	}

	def Add(item:ScheduleItem)
	{
		items=items:+item
	}

	def ShutDown
	{
		Save
	}
}

// Scheduler is responsible for creating a schedule from selected engines and setup
// and organize games to be played from this schedule
case class Scheduler(
	enginelist:GEngineList,
	enginegames:EngineGames,
	SelectEngineGamesTab:()=>Unit
)
{
	var selectedengines=SelectedEngines(enginelist)

	var schedulesetup=ScheduleSetup()

	var schedule=Schedule()

	var scheduler_thread:Thread=null

	def SelectEngines
	{
		selectedengines.Query
	}

	def GameSetup
	{
		schedulesetup.ShowScheduleSetup
	}

	def handler(ev:MyEvent)
	{		
		if(ev.kind=="webview clicked")
		{
			if(ev.id=="scheduletext")
			{
				val we=Builder.getwebe("scheduletext")
				val command=we.executeScript("command").toString()
				
				if(command=="stop")
				{
					Stop
				}
				if(command=="start")
				{
					Start
				}
				if(command=="create")
				{
					CreateSchedule
				}
			}
		}
	}

	def UpdateSchedule
	{		
		Builder.setweb("scheduletext",schedule.ReportScheduleHTML(schedule.ptr-2,schedule.items.length-1))
		Builder.setweb("resultstext",schedule.ReportResultsHTML)
		Builder.setweb("standingstext",schedule.ReportStandingsHTML)
	}

	def IsStageOpen:Boolean=Builder.stages.contains("schedulestage")

	def ToTop
	{
		if(IsStageOpen)
		{
			Builder.stages("schedulestage").ToTop
		}	
	}

	def CreateStage
	{
		val blob=s"""			
			|<tabpane>
			|<tab caption="Schedule">
			|<scrollpane id="schedulescrollpane" width="600.0" height="645.0">
			|<webview id="scheduletext" height="3000.0" width="3000.0"/>
			|</scrollpane>
			|</tab>
			|<tab caption="Results">
			|<scrollpane id="resultsscrollpane" width="600.0" height="645.0">
			|<webview id="resultstext" height="3000.0" width="3000.0"/>
			|</scrollpane>
			|</tab>
			|<tab caption="Standings">
			|<scrollpane id="standingsscrollpane" width="600.0" height="645.0">
			|<webview id="standingstext" height="3000.0" width="3000.0"/>
			|</scrollpane>
			|</tab>
			|</tabpane>
		""".stripMargin
		Builder.MyStage("schedulestage",modal=false,set_handler=handler,title="Scheduler",blob=blob)
	}

	def ShowSchedule
	{
		
		if(!IsStageOpen) CreateStage
		ToTop
		UpdateSchedule
	}

	def CreateSchedule
	{
		schedule.Clear

		if(!schedulesetup.HasItems)
		{
			// need at least one starting condition to create a schedule
			schedulesetup.Add(ScheduleSetupItem())
		}

		// add schedule for every starting condition
		for(setupitem<-schedulesetup.items)
		{
			// select the white player
			for((k,playerwhite)<-selectedengines.items)
			{
				// select the black player
				for((k,playerblack)<-selectedengines.items)
				{
					// no engine should play against itself
					if(playerwhite!=playerblack)
					{
						// only selected engine should take part
						if((playerwhite.selected)&&(playerblack.selected))
						{							
							val scheduleitem=ScheduleItem(
								playerwhitepathid=playerwhite.pathid,
								playerwhitename=playerwhite.name,
								playerblackpathid=playerblack.pathid,
								playerblackname=playerblack.name,
								fen=setupitem.fen,
								thematicmoves=setupitem.thematicmoves
							)
							schedule.Add(scheduleitem)							
						}
					}
				}
			}
		}

		ShowSchedule
	}

	var interrupted=false

	val time_step=1000
	var gamestarted=false
	def scheduler_thread_func
	{
		while((!Thread.currentThread.isInterrupted())&&(!interrupted))
		{
			if(schedule.Done)
			{
				interrupted=true
			}
			else
			{
				if(enginegames.gamerunning)
				{

				}
				else
				{
					if(gamestarted)
					{
						// a game started by the scheduler has finished
						if(enginegames.gameresult!=null)
						{							
							val result=enginegames.gameresult.resultstr
							val current=schedule.Current
							if(current!=null)
							{								
								current.SetResult(result)
								schedule.Advance
							}
							gamestarted=false
							try{Thread.sleep(8000)}catch{case e:Throwable=>{}}
						}
					}
					else
					{
						// time to start a game
						val current=schedule.Current
						if(current!=null)
						{
							enginelist.UnloadAll
							val succb=enginelist.BringUp(current.playerblackpathid)
							val succw=enginelist.BringUp(current.playerwhitepathid)
							if(succb&&succw)
							{
								// engines brought to top and loaded ok
								Platform.runLater(new Runnable{def run{
									SelectEngineGamesTab()
								}})
								try{Thread.sleep(3000)}catch{case e:Throwable=>{}}
								commands.g.reset
								val variant=settings.getvariant
								val fen=current.fen
								val movelist=current.thematicmoves
								val pgn=s"""
									|[FEN "$fen"]
									|[Variant "$variant"]
									|
									|$movelist
								""".stripMargin								
								commands.g.set_from_pgn(pgn)
								commands.g.toend
								enginegames.StartGame(fromposition=true,forced=true)
								gamestarted=true
							}
							else
							{
								// cannot load engines interrupt
								SystemMessage.Show("Scheduler message","Cannot load engines.","Scheduler stopped.",popup=true,runlater=true)
								interrupted=true
							}
						}
					}
				}
				Platform.runLater(new Runnable{def run{
					UpdateSchedule
				}})
				try{Thread.sleep(time_step)}catch{case e:Throwable=>{}}
				SchedulerGlobals.elapsed+=time_step
			}
		}
		
		SchedulerGlobals.running=false
		gamestarted=false
		Platform.runLater(new Runnable{def run{
			UpdateSchedule
		}})
	}

	def Show
	{
		ShowSchedule
	}

	def GameRunningMessage(message:String)
	{
		SystemMessage.Show("Scheduler message",message,comment="Game is running.",popup=true,runlater=true)
	}

	def Start
	{	

		if(SchedulerGlobals.running)
		{			
			ShowSchedule
			return
		}

		if(enginegames.gamerunning)
		{
			GameRunningMessage("Cannot start scheduler.")
			return
		}

		ShowSchedule

		scheduler_thread=new Thread(new Runnable{def run{
			scheduler_thread_func
		}})

		interrupted=false

		SchedulerGlobals.elapsed=0

		scheduler_thread.start

		SchedulerGlobals.running=true
	}

	def Stop
	{
		ToTop

		if(scheduler_thread!=null)
		{
			if(SchedulerGlobals.running)
			{
				scheduler_thread.interrupt()
				interrupted=true
			}
		}
	}

	def Load
	{
		selectedengines.Load

		schedulesetup.Load

		schedule.Load
	}

	def ShutDown
	{
		selectedengines.ShutDown

		schedulesetup.ShutDown

		schedule.ShutDown

		Stop
	}
}

////////////////////////////////////////////////////////////////////