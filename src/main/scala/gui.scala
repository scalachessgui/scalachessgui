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

import java.awt.image._

////////////////////////////////////////////////////////////////////

import java.io._

import org.apache.commons.io.FileUtils._
import org.apache.commons.lang.time.DurationFormatUtils.formatDuration

////////////////////////////////////////////////////////////////////

import builder._
import guiboard._
import commands._
import board._
import square._
import move._
import settings._
import components._
import book._
import piece._
import game._

import gui2.Engine
import gui2.Robot

////////////////////////////////////////////////////////////////////

class GuiClass extends Application
{

	val scrollpanes=List("colorpgn","pgn","pgntree","move","engine","engines","enginegames")

	var enginelist:GEngineList=null

	var enginegames=EngineGames(DisableBoardControls,EnableBoardControls,GetEngineList,GuiUpdate,addcurrentgametobook)

	def getpanewidth=Builder.gsd("panewidth",750.0)
	def getinnerpanewidth=getpanewidth-30.0

	var boardcontrolsdisabled=false

	def GuiUpdate()
	{
		update
	}

	def ResetGame
	{
		commands.exec("r")

		update
	}

	def DisableBoardControls()
	{
		boardcontrolsdisabled=true
		if(gb==null) return
		gb.DisableControls
	}

	def EnableBoardControls()
	{
		boardcontrolsdisabled=false
		if(gb==null) return
		gb.EnableControls
	}

	def GetEngineList():GEngineList=
	{
		return enginelist
	}

	def eval_exp(exp:String):String=
	{
		val res=exp match
		{
			case "panewidth" => ""+getpanewidth
			case "innerpanewidth" => ""+getinnerpanewidth
			case _ => ""
		}

		res
	}

	def apply_setup(fen:String)
	{
		commands.g.set_from_fen(fen)

		update

		Builder.stages("setupboard").close
	}

	def gettablist:Tuple2[List[String],TabPane]=
	{
		val tabspane=Builder.getcomp("tabs").node.asInstanceOf[TabPane]

		val tabs=tabspane.getTabs()

		Tuple2((for(i<-0 to tabs.size-1) yield tabs.get(i).getText()).toList,tabspane)
	}

	def selecttab(tabname:String)
	{
		val tablist=gettablist

		val i=tablist._1.indexOf(tabname)

		if(i< 0) return

		tablist._2.getSelectionModel().select(i)
	}

	def choose_pgn_file:File=
	{
		val fc=new FileChooser()

		if(new File(settings.pgn_dir).exists)
		{
			fc.setInitialDirectory(new File(settings.pgn_dir))
		}

		val f=fc.showOpenDialog(new Stage())

		if(f!=null)
		{
			val dir=f.getParent()

			settings.pgn_dir=dir
		}

		f
	}

	var selected_build_pgn:File=null

	def buildp(sort_only:Boolean=false,build_pgn:String=null)
	{

		if(!sort_only)
		{
			val book=Builder.gettext("buildinbook").getText

			if(book=="") return

			commands.exec(s"scb $book")

			update
		}

		var pgn=build_pgn

		var name="unknown source"

		if(pgn==null)
		{

			val f=selected_build_pgn

			if(f==null) return

			name=f.getName()

			pgn=readFileToString(
						f,
						null.asInstanceOf[String]
					)

		}

		var ld:components.LogDialog=null

		def op()
		{
			if(sort_only)
			{
				commands.g.sort_pgn(pgn)
			}
			else
			{
				commands.g.build_book(pgn)
			}

			ld.s.close
		}

		def cancel()
		{
			commands.g.interrupted=true
		}

		ld=new components.LogDialog(s"Processing $name",set_cancel_callback=cancel,set_func=op)

		commands.g.log_callback=ld.lp.add

		//g.log_callback=println

		ld.start

		selecttab(if(sort_only) "Build" else "Book")

		update
	}

	def select_build_pgn()
	{
		val fc=new FileChooser()

		if(new File(settings.build_dir).exists)
		{
			fc.setInitialDirectory(new File(settings.build_dir))
		}

		val f=fc.showOpenDialog(new Stage())

		if(f!=null)
		{
			val dir=f.getParent()

			settings.build_dir=dir

			//println("build dir set to "+dir)

			selected_build_pgn=f

			val name=f.getName()

			Builder.getlabel("selectedbuildpgn").setText(name)

			val name_parts=name.split("\\.")
			var book=name_parts(0)
			Builder.gettext("buildinbook").setText(book)
		}
	}

	var pgn_game_browser:components.GameBrowser=null
	var book_game_browser:components.GameBrowser=null
	var current_book_ecombo:components.EditableCombo=null

	def scb(cb:String)
	{
		val c=s"scb $cb"
		//println(c)
		commands.exec(c)
		update
	}

	def delb(db:String)
	{
		var ld:components.LogDialog=null

		def op()
		{
			commands.exec(s"delb $db")

			ld.s.close
		}

		def cancel()
		{
			butils.interrupted=true

			//ld.s.close
		}

		ld=new components.LogDialog(s"Deleting book $db",set_cancel_callback=cancel,set_func=op)

		butils.log_callback=ld.lp.add

		ld.start

		current_book_ecombo.c.create_from_list(butils.list_books())
		current_book_ecombo.c.select("default",set_trigger=true)

		update
	}

	def open_mult_pgn()
	{

		selected_build_pgn=choose_pgn_file

		buildp(sort_only=true)

		pgn_game_browser.game_list=commands.g.pgn_games

		pgn_game_browser.update

		selecttab("PGN Games")
	}

	def open_pgn()
	{

		val f=choose_pgn_file

		if(f!=null)
		{
			val name=f.getName()

			Builder.setsval("lastpgnname",name)

			val pgn=readFileToString(f,null.asInstanceOf[String])

			commands.g.set_from_pgn(pgn)

			selecttab("Color PGN")
			commands.exec("ff")

			update
		}

	}

	var confirm_callback:(Boolean)=>Unit=null

	def save_pgn_as
	{
		Builder.closeStage("savepgnas")

		var name=Builder.gettext("savepgnasname").getText

		if(name=="")
		{
			Builder.setcval("message","File could not be saved. Name empty.")
			Builder.MyStage("infodialog",modal=true,set_handler=handler,title="Error")
			return
		}

		val path=Builder.gettextarea("savepgnasdir").getText

		val name_parts=name.split("\\.").toList

		if(name_parts.length>0)
		{
			if(name_parts.reverse.head!="pgn")
			{
				name=name+".pgn"
			}
		}
		else
		{
			name=name+".pgn"
		}

		name=name.replaceAll("[\\.]+",".")

		var fullpath=""

		fullpath=path+File.separator+name

		val f=new File(fullpath)

		if(f.exists)
		{
			Builder.setcval("message","Overwrite?")
			confirm_callback=overwrite
			Builder.MyStage("confirmdialog",modal=true,set_handler=handler,title="File already exists")
		}
		else
		{
			overwrite(true)
		}

		def overwrite(go:Boolean)
		{

			if(go)
			{

				try
				{					

					writeStringToFile(f,commands.g.report_pgn)

				}
				catch
				{
					case e: Throwable =>
					{
						Builder.setcval("message","File could not be saved. IO error.")
						Builder.MyStage("infodialog",modal=true,set_handler=handler,title="Error")
						return
					}
				}

				fullpath=f.getAbsolutePath()

				Builder.setsval("lastpgnname",name)

				settings.pgn_dir=path

				Builder.setcval("message",s"File saved to $fullpath.")
				Builder.MyStage("infodialog",modal=true,set_handler=handler,title="Ok")

			}

		}
	}

	def addmove
	{
		if(commands.g.current_node!=commands.g.root)
		{
			val san=commands.g.current_node.genSan
			commands.g.back
			val old_inc=settings.inc_move_count
			settings.inc_move_count=true
			commands.g.makeSanMove(san)
			settings.inc_move_count=old_inc
			commands.g.back
			update
		}
	}

	def enginesclicked
	{
		enginelist.Handle
	}

	def bookclicked
	{
		val key=Builder.getwebe("booktext").executeScript("key").toString()
		val action=Builder.getwebe("booktext").executeScript("action").toString()
		val param=Builder.getwebe("booktext").executeScript("param").toString()

		if(action=="annot")
		{
			commands.exec(s"a $key $param")
			update_book_text
		}
		else if(action=="make")
		{
			commands.exec(s"m $key")
			update
		}
		else if(action=="del")
		{
			commands.exec(s"del $key")
			update
		}
		else if(action=="comment")
		{
			commentedsan=key
			Builder.MyStage("bookcomment",modal=true,set_handler=handler,title="Add book comment")			
		}
	}

	var commentedsan:String=""

	def gen_random(num:Int)
	{
		val r=new scala.util.Random()

		var ok=true

		while(ok)
		{

			var cnt=0

			while((cnt< num)&&ok)
			{

				commands.g.b.genMoveList

				val isforward=false

				val move_list=(for(m<-commands.g.b.move_list if
				(
					((commands.g.b.turn==piece.WHITE)&&(square.rankOf(m.to)< square.rankOf(m.from)))
					||
					((commands.g.b.turn==piece.BLACK)&&(square.rankOf(m.to)> square.rankOf(m.from)))
					||
					(!isforward)
				)) yield m).toList

				val len=move_list.length

				if(len>0)
				{
					
					val i=r.nextInt(len)

					val m=move_list(i)

					commands.g.makeMove(m)

					cnt+=1

				}
				else
				{

					ok=false

				}

			}

			ok=false

		}

		update
	}

	def openenginesettings
	{
		Builder.MyStage("engineoptions",modal=true,do_size=false,set_handler=handler,title="Engine settings")
	}

	def learn_start
	{
		learn_on=true
		new Thread(new Runnable{
			def run{
				learn_thread_func
			}
		}).start()
	}

	def IssueEngineCommand
	{
		val etext=Builder.gettext("enginecommand")
		val command=etext.getText
		etext.setText("")
		engine.issue_command(command+"\n")
	}

	def StyleTimecontrols
	{
		val isconventional=Builder.gb("components#timecontrolconventional",true)
		val borderstyle="-fx-border-style: solid; -fx-border-width: 1px; -fx-border-radius: 15px;"
		val selectedstyle="-fx-background-color: #afffaf;"
		val notselectedstyle="-fx-background-color: #afafaf;"

		val conventionaloutervbox=Builder.getvboxn("conventionaloutervbox")
		val conventionalvbox=Builder.getvboxn("conventionalvbox")
		val incrementaloutervbox=Builder.getvboxn("incrementaloutervbox")
		val incrementalvbox=Builder.getvboxn("incrementalvbox")

		if(isconventional)
		{
			conventionalvbox.setStyle(selectedstyle)
			incrementalvbox.setStyle(notselectedstyle)
		} else {
			conventionalvbox.setStyle(notselectedstyle)
			incrementalvbox.setStyle(selectedstyle)
		}

		conventionaloutervbox.setStyle(borderstyle)
		incrementaloutervbox.setStyle(borderstyle)
	}

	case class EngineStatsItem(
		white:String,
		black:String,
		result:String
	)
	{
		def pairing_key:String=s"$white vs. $black"
	}

	case class Elos(
		var elos:Map[String,Double]=Map[String,Double]()
		)
	{
		def Update(item:EngineStatsItem)
		{
			// https://en.wikipedia.org/wiki/Elo_rating_system

			val BASE_ELO=2800.0

			var whiteelo=BASE_ELO
			if(elos.contains(item.white)) whiteelo=elos(item.white)

			var blackelo=BASE_ELO
			if(elos.contains(item.black)) blackelo=elos(item.black)
			
			def ESCORE(A:Double,B:Double):Double=1.0/(1.0+scala.math.pow(10.0,(B-A)/400.0))

			var scorewhite:Double=0.5
			var scoreblack:Double=0.5

			if(item.result=="1-0") { scorewhite=1.0; scoreblack=0.0 }
			else if(item.result=="0-1") { scorewhite=0; scoreblack=1.0 }

			val ESCOREW = ESCORE(whiteelo,blackelo)
			val ESCOREB = ESCORE(blackelo,whiteelo)

			val K=32.0

			whiteelo=(whiteelo.toDouble+K*(scorewhite-ESCOREW))
			blackelo=(blackelo.toDouble+K*(scoreblack-ESCOREB))

			elos+=(item.white->whiteelo)
			elos+=(item.black->blackelo)
			
		}

		def ReportEloHTML(player:String,elo:Double):String=
		{
			val elof="%.2f".format(elo)
			s"""
				|<tr>
				|<td><font size="4" color="#00007f">$player</font></td>
				|<td><font size="5">$elof</font></td>
				|</tr>
			""".stripMargin
		}

		def ReportHTML:String=
		{
			val sortedkeys=elos.keys.toList.sortWith( elos(_) > elos(_) )
			val eloscontent=(for(k<-sortedkeys) yield ReportEloHTML(k,elos(k))).mkString("\n")
			s"""
				|<table cellpadding="10" cellspacing="10" border="1">
				|<tr>
				|<td>Engine</td>
				|<td>ELO</td>
				|</tr>
				|$eloscontent
				|</table>
			""".stripMargin
		}
	}

	case class PairingStats(
		var whitewins:Int=0,
		var draw:Int=0,
		var blackwins:Int=0,
		var white:String="",
		var black:String="",
		var key:String=""
		)
	{
		def ReportHTML:String=
		{
			s"""
				|<tr>
				|<td>
				|<font size="4" color="#007f00"><b>$white</b></font>
				| vs. 
				|<font size="4" color="#7f0000"><b>$black</b></font>
				|</td>
				|<td align="center"><font size="6" color="#007f00"><b>$whitewins</b></font></td>
				|<td align="center"><font size="6" color="#00007f"><b>$draw</b></font></td>
				|<td align="center"><font size="6" color="#7f0000"><b>$blackwins</b></font></td>
				|</tr>
			""".stripMargin
		}
	}

	case class EngineStats(
		var items:List[EngineStatsItem]=List[EngineStatsItem](),
		var pairings:Map[String,PairingStats]=Map[String,PairingStats]()
		)
	{
		def Add(item:EngineStatsItem)
		{
			items=items:+item

			val key=item.pairing_key
			var pstat=PairingStats()
			if(pairings.contains(key))
			{
				pstat=pairings(key)
			}
			if(item.result=="1-0") pstat.whitewins+=1
			else if(item.result=="1/2-1/2") pstat.draw+=1
			else if(item.result=="0-1") pstat.blackwins+=1
			pstat.white=item.white
			pstat.black=item.black
			pstat.key=key
			pairings+=(key->pstat)


		}

		def ReportHTML:String=
		{
			val pairingscontent=(for((k,v)<-pairings) yield v.ReportHTML).mkString("\n")
			s"""
				|<table cellpadding="5" cellspacing="5" border="1">
				|<tr>
				|<td>Pairing</td>
				|<td>White wins</td>
				|<td>Draw</td>
				|<td>Black wins</td>
				|</tr>
				|$pairingscontent
				|</table>
			""".stripMargin
		}
	}

	def create_engine_game_stats
	{
		val stats=EngineStats()
		val elos=Elos()
		val dummy=new game
		val gf=new File("enginegames.pgn")
		if(gf.exists())
		{
			val pgncontent=readFileToString(gf,null.asInstanceOf[String])
			val pgns=dummy.split_pgn(pgncontent)
			for(pgn<-pgns)
			{
				dummy.parse_pgn(pgn,head_only=true)
				val white=dummy.get_header("White")
				val black=dummy.get_header("Black")
				val result=dummy.get_header("Result")
				val item=EngineStatsItem(white,black,result)
				stats.Add(item)
				elos.Update(item)
			}			
		}
		Builder.setweb("enginegamestext",stats.ReportHTML+"<br><br>\n"+elos.ReportHTML)
		selecttab("Engine games")
	}

	def addcurrentgametobook()
	{
		val book_enabled=Builder.gcb("bookenabled",true)
		if(!book_enabled)
		{
			SystemMessage.Show("Adding game to book failed","Cannot add game to book.","Book is not enabled.")
			SystemMessage.Hide()
			return
		}
		SystemMessage.Show("Adding game to book","Adding game to book ...","Please wait.")
		commands.g.log_callback=commands.g.default_log_callback
		val pgn=commands.g.report_pgn
		commands.g.build_book(pgn)
		commands.g.set_from_pgn(pgn)
		commands.g.toend
		SystemMessage.Hide(3000)
	}

	def change_multipv
	{
		val running=engine.engine_running
		if(running) engine_stop

		set_multipv(get_multipv)
		
		if(running) engine_start
	}

	def edit_pgn(evid:String="editpgnok")
	{
		val field=Builder.gettext("pgnfieldname").getText

		if(field!="")
		{
			if(evid=="editpgndel")
			{
				commands.g.pgn_headers-=field
			}
			else
			{
				val value=Builder.gettext("pgnfieldvalue").getText

				commands.g.pgn_headers+=(field->value)
			}

			update
		}

		Builder.stages("editpgn").close
	}

	def edit_pgn_comment(evid:String="editpgncommentok")
	{
		var comment=Builder.gettext("pgncomment").getText

		if(evid=="editpgncommentdel") comment=""

		commands.g.current_node.comment=comment

		Builder.stages("editpgncomment").close

		update
	}

	def handler(ev:MyEvent)
	{
		Builder.default_handler(ev)

		if(ev.kind=="stage closed")
		{
			if(ev.id=="main")
			{
				Builder.CloseAllStages
			}
		}
		
		if(ev.kind=="checkbox changed")
		{
			if(ev.id=="timecontrolconventional")
			{
				val cv=Builder.gcb("timecontrolconventional",true)
				Builder.setcheckgc("timecontrolincremental",!cv)
				StyleTimecontrols
			}

			if(ev.id=="timecontrolincremental")
			{
				val cv=Builder.gcb("timecontrolincremental",false)
				Builder.setcheckgc("timecontrolconventional",!cv)
				StyleTimecontrols
			}

			if(ev.id=="bookenabled")
			{
				commands.g.pos_changed
				update
			}
		}

		if(ev.kind=="combo selected")
		{
			if(ev.id=="enginescombo")
			{
				engine_selected
			}

			if(ev.id=="multipvcombo")
			{
				change_multipv
			}

			if(ev.id=="selectvariantcombo")
			{
				variant_selected()
			}
		}

		if(ev.kind=="webview clicked")
		{

			if(ev.id=="enginegamestext")
			{
				val we=Builder.getwebe("enginegamestext")
				val command=we.executeScript("command").toString()
				if(command=="abort")
				{
					enginegames.AbortGame
				}
				if(command=="start")
				{
					ResetGame
					enginegames.StartGame(fromposition=false)
				}
				if(command=="startfrompos")
				{
					enginegames.StartGame(fromposition=true)
				}
			}

			if(ev.id=="enginestext")
			{
				enginesclicked
			}

			if(ev.id=="booktext")
			{
				bookclicked
			}

			if(ev.id=="colorpgntext")
			{
				val index_str=Builder.getwebe("colorpgntext").executeScript("x").toString()

				if(index_str!="")
				{

					if(index_str=="edit")
					{
						val field=Builder.getwebe("colorpgntext").executeScript("field").toString()

						val value=commands.g.get_header(field)

						Builder.setcval("pgnfieldname",field)
						Builder.setcval("pgnfieldvalue",value)

						Builder.MyStage("editpgn",modal=true,set_handler=handler,title="Edit PGN field")
					}
					else
					{
						val index=index_str.toInt

						val gn=commands.g.html_pgn_nodes(index)

						val action=Builder.getwebe("colorpgntext").executeScript("action").toString()

						if(action=="editcomment")
						{
							val comment=gn.comment
							val blob=s"""
								|<vbox padding="15" gap="5">
								|<label text="Comment:"/>
								|<textfield style="-fx-font-size: 24px;" id="pgncomment" text="$comment" width="400"/>
								|<button style="-fx-font-size: 24px;" id="editpgncommentok" width="400" text="Apply changes"/>
								|<button id="editpgncommentdel" text="Delete this comment"/>
								|</vbox>
							""".stripMargin
							Builder.MyStage("editpgncomment",modal=true,set_handler=handler,title="Edit PGN comment",blob=blob)
						}
						else
						{
	            			commands.g.tonode(gn)
						}


	            		update
            		}
				}
			}

			if(ev.id=="movetext")
			{
				val san=Builder.getwebe("movetext").executeScript("x").toString()
				if(san!="")
				{
					val m=commands.g.b.sanToMove(san)
					if(m!=null)
					{
						manual_move_made(m,san)
					}
				}
			}
			
		}

		if(ev.kind=="menuitem clicked")
		{

			if(ev.id=="enginegamestats")
			{
				create_engine_game_stats
			}

			if(ev.id=="addcurrentgametobook")
			{
				addcurrentgametobook
			}

			if(ev.id=="booksettings")
			{
				Builder.MyStage("booksettings",modal=true,set_handler=handler,do_size=false,title="Book settings")
			}

			if(ev.id=="enginegamesettings")
			{
				Builder.MyStage("enginegamesettings",modal=true,set_handler=handler,do_size=false,title="Engine game settings")
			}

			val rmatch="""random([0-9]+)""".r

			ev.id match
			{				
				case rmatch(num_halfmoves) => gen_random(num_halfmoves.toInt)

				case _=>
			}

			if(ev.id=="engineconsole")
			{
				Builder.MyStage("engineconsole",modal=false,set_handler=handler,do_size=false,title="Engine console")
			}

			if(ev.id=="startenginegame")
			{
				if(!enginegames.gamerunning)
				{
					ResetGame
					enginegames.StartGame(fromposition=false)
					selecttab("Engine games")
				}
			}

			if(ev.id=="startenginegamefromcurrentposition")
			{
				if(!enginegames.gamerunning)
				{
					enginegames.StartGame(fromposition=true)
					selecttab("Engine games")
				}
			}

			if(ev.id=="abortenginegame")
			{
				enginegames.AbortGame
			}

			if(ev.id=="enginegamestimecontrol")
			{
				Builder.MyStage("enginegamestimecontrol",modal=true,do_size=false,set_handler=handler,title="Engine games - time control")
				StyleTimecontrols
			}

			if(ev.id=="recordrect")
			{
				Builder.MyStage("recordrectdialog",modal=true,set_handler=handler,title="Record rect")
			}

			if(ev.id=="recordcol")
			{
				learn_board_colors
			}

			if(ev.id=="openmultpgn")
			{
				open_mult_pgn
			}

			if(ev.id=="loadengine")
			{
				load_engine
			}

			if(ev.id=="openpgn")
			{
				open_pgn
			}

			if(ev.id=="enginesettings")
			{
				openenginesettings
			}

			if(ev.id=="savepgnas")
			{
				Builder.setcval("savepgnasdir",settings.pgn_dir)

				Builder.setcval("savepgnasname",Builder.gss("lastpgnname",""))

				Builder.MyStage("savepgnas",modal=true,set_handler=handler,title="Save PGN as")
			}

			if(ev.id=="copyfen")
			{
				ClipboardSimple.clipset(commands.g.report_fen)
			}

			if(ev.id=="copypgn")
			{
				ClipboardSimple.clipset(commands.g.report_pgn)
			}

			if(ev.id=="copycurrentline")
			{
				ClipboardSimple.clipset(commands.g.current_line_pgn)
			}

			if(ev.id=="pastefen")
			{
				commands.g.set_from_fen(ClipboardSimple.clipget)
				update
			}

			if(ev.id=="pastepgn")
			{
				commands.g.set_from_pgn(ClipboardSimple.clipget)
				update
			}

			if(ev.id=="boardsettings")
			{
				Builder.MyStage("boardsettings",modal=true,set_handler=handler,do_size=false,title="Board settings")
			}

			if(ev.id=="setupboard")
			{
				val s=Builder.MyStage("setupboard",modal=true,do_build=false,do_size=false,do_show=false,title="Setup board")

				val sb=new setupboard(apply_callback=apply_setup)

				sb.set_from_fen(commands.g.report_fen)

				val p=Builder.build(Builder.dpath("setupboard"),handler)

				Builder.getcomp("setupboardvbox").node.asInstanceOf[VBox].getChildren().add(sb.root)

				s.setscene(p)

				s.show
			}
		}

		if(ev.kind=="profile applied")
		{
			if(ev.id=="bookcommentprofile")
			{
				Builder.closeStage("bookcomment")
				val sel=Builder.getcombo("bookcommentprofileselect").get_selected

				if(sel!="")
				{
					commands.g.book.currentPos.comment(commentedsan,sel)
					commands.g.book.savePos()

					update
				}
			}

			if(ev.id=="boardprofile")
			{
				build_board
			}
		}

		if(ev.kind=="textfield entered")
		{
			if(ev.id=="enginecommand")
			{
				IssueEngineCommand
			}

			if(ev.id=="pgncomment")
			{
				edit_pgn_comment()
			}

			if((ev.id=="pgnfieldname")||(ev.id=="pgnfieldvalue"))
			{
				edit_pgn()
			}
		}

		if(ev.kind=="button pressed")
		{

			if(ev.id=="applymultipv")
			{
				change_multipv
			}

			if(ev.id=="issueenginecommand")
			{
				IssueEngineCommand
			}

			if(ev.id=="learnboard")
			{
				if(learn_on)
				{
					learn_on=false
				}
				else
				{
					learn_start
				}
			}

			val parts=ev.id.split("!").toList

			if(ev.id=="applyallandclose"){
				for(id<-allengineids){
					handler(builder.MyEvent(id=id+"!apply",kind="button pressed"))
					Builder.stages("engineoptions").close
				}
			}

			if(parts.length==2)
			{
				val action=parts(1)
				if(action=="enginecommand")
				{
					val command=parts(0)
					if(command=="setfen")
					{
						engine.issue_command("position fen "+commands.g.b.report_fen+"\n")
					} else {
						engine.issue_command(command+"\n")
					}
				}
				if((action=="apply")||(action=="applyclose"))
				{
					val id=parts(0)
					var comp=Builder.getcomp(id)
					if(comp==null) comp=Builder.getcomp(ev.id)
					if(comp!=null)
					{
						if(comp.isInstanceOf[Builder.MyTextField])
						{
							Builder.setcval(id,comp.asInstanceOf[Builder.MyTextField].getText)
						}
						var value=Builder.getcvals(id,"")
						if(comp.isInstanceOf[Builder.MySlider])
						{
							value=""+data.Utils.parse[Double](value,0.0).toInt
						}
						val idparts=id.split("#")
						var name=idparts(0)
						if(idparts.length>0) name=idparts(idparts.length-1)
						val command=s"setoption name $name value $value"
						println("engine command "+command)
						engine.issue_command(command+"\n")
						if(action=="applyclose")
						{
							Builder.stages("engineoptions").close
						}
					}
				}
			}

			if(ev.id=="selectbuildpgn")
			{
				select_build_pgn
			}

			if(ev.id=="sortbyvariants")
			{
				buildp(sort_only=true)
			}

			if(ev.id=="buildpgn")
			{
				buildp()
			}

			if((ev.id=="addmove")&&(commands.g.book_enabled))
			{
				addmove
			}

			if(ev.id=="boardcontrolpaneloptions")
			{
				openenginesettings
			}

			if(ev.id=="boardcontrolpanelstart")
			{
				if(boardcontrolsdisabled) return
				engine_start
			}

			if(ev.id=="boardcontrolpanelstop")
			{
				if(boardcontrolsdisabled) return
				engine_stop
			}

			if(ev.id=="boardcontrolpanelmake")
			{
				if(boardcontrolsdisabled) return
				engine_make
			}

			val hmatch="""boardcontrolpanelhint([0-9]+)""".r

			ev.id match
			{				
				case hmatch(dur) => engine_hint(dur.toInt)

				case _=>
			}

			if(ev.id=="selectpgnsaveasdir")
			{
				val dc=new DirectoryChooser()

				if(new File(settings.pgn_dir).exists)
				{
					dc.setInitialDirectory(new File(settings.pgn_dir))
				}

				val f=dc.showDialog(new Stage())

				if(f!=null)
				{
					val path=f.getPath()

					Builder.setsval("savepgnasdir",path)

					Builder.gettextarea("savepgnasdir").setText(path)

					settings.pgn_dir=path
				}
			}

			if(ev.id=="confirmyes")
			{
				Builder.closeStage("confirmdialog")
				if(confirm_callback==null) return
				confirm_callback(true)
				confirm_callback=null
			}

			if(ev.id=="confirmno")
			{
				Builder.closeStage("confirmdialog")
				if(confirm_callback==null) return
				confirm_callback(false)
				confirm_callback=null
			}

			if(ev.id=="messageok")
			{
				Builder.closeStage("infodialog")
			}

			if(ev.id=="dosavepgnas")
			{
				save_pgn_as
			}

			if((ev.id=="editpgncommentok")||(ev.id=="editpgncommentdel"))
			{
				edit_pgn_comment(ev.id)
			}

			if((ev.id=="editpgnok")||(ev.id=="editpgndel"))
			{
				edit_pgn(ev.id)
			}

			if(ev.id=="boardcontrolpanelflip")
			{
				settings.flip=(!settings.flip)

				update
			}

			if(ev.id=="boardcontrolpanelreset")
			{
				if(boardcontrolsdisabled) return

				ResetGame
			}

			if(ev.id=="boardcontrolpaneldel")
			{
				if(boardcontrolsdisabled) return

				commands.exec("d")

				update
			}

			if(ev.id=="boardcontrolpaneltobegin")
			{
				commands.exec("bb")

				update
			}

			if(ev.id=="boardcontrolpanelback")
			{
				commands.exec("b")

				update
			}

			if(ev.id=="boardcontrolpanelforward")
			{
				commands.exec("f")

				update
			}

			if(ev.id=="boardcontrolpaneltoend")
			{
				commands.exec("ff")

				update
			}
		}
	}

	var gb:guiboard=null

	val engine=new components.MyEngine(update_engine,update_log)

	def get_multipv:Int=
	{
		Builder.gi("components#multipvcombo#selected",3)
	}

	var elog=Array[String]()

	def update_log(setwhat: String)
	{
		var what=setwhat
		what=what.replaceAll("\\n","")
		if(what=="<- ") return
		if(what(0)=='\r')
		{
			if(elog.length>0)
			{
				if(elog(elog.length-1).contains("\r"))
				{
					elog(elog.length-1)=what
				} else {
					elog=elog:+what
				}
			} else {
				elog=elog:+what
			}
		}
		else
		{
			elog=elog:+what
		}
		while(elog.length>38) {
			elog=elog.tail
		}
		def formatline(setwhat: String): String=
		{
			var what=setwhat
			if(what.length < 3) return what+"<br>"
			val pref=what.substring(0,3)
			var line=what.substring(3)
			line=line.replaceAll(" ","&nbsp;")
			if(pref=="-> ")
			{
				return s"""<font color="red">$line</font><br>"""
			} else if(pref=="<- ") {
				return s"""<font color="blue">$line</font><br>"""
			} else {
				return s"""<font color="brown">$what</font><br>"""
			}
		}
		if(Builder.stages.contains("engineconsole"))
		{
			val eloghtml=elog.map(formatline)
			val elogcontent=eloghtml.mkString
			val content=s"""
				|<div style="font-family: monospace;">
				|$elogcontent
				|</div>
			""".stripMargin
			Platform.runLater(new Runnable{def run{
				Builder.getwebe("engineconsoletext").loadContent(content)
			}})
		}
	}

	def update_engine()
	{
		Platform.runLater(new Runnable{
			def run
			{

				val score=engine.score_numerical

				gb.print_score(score)

				gb.highlight_engine_move(engine.pv,score)

				val mpv=get_multipv

				val pvs=(for(i<-1 to mpv) yield
				{
					val el=engine.lines(i-1)
					val pv=el.pvs
					val pvparts=pv.split(" ").toList
					val san=pvparts(0)
					val line=if(pvparts.length>0) pvparts.tail.mkString(" ") else ""
					val score=el.score_numerical
					val score_color=if(score>=0) "#007f00" else "#7f0000"
					val score_sign=if(score>0) "+" else ""

					s"""
					|$i.</td>
					|<td><b><font size=6 color="#0007f">$san</font></b></td>
					|<td><b><font size=6 color=$score_color>$score_sign$score</font></b></td>
					|<td>$line
					""".stripMargin
				}).mkString("</td></tr><tr><td>")

				val depth=engine.depth					
				val nodes=engine.nodes
				val nodesf="%.2f".format(nodes/1.0e6)
				val nps=engine.nps
				val npsf="%.2f".format(nps/1.0e3)
				val time=engine.time
				val timef=formatDuration(time,"HH:mm:ss")

				val engine_path=settings.get_variant_engine_path()

				val f=new File(engine_path)

				var engine_name=""

				if(f.exists)
				{

					val engine_full_name=f.getName()

					val engine_full_name_parts=engine_full_name.split("\\.").toList

					engine_name=engine_full_name_parts.head

				}

				val content=s"""
				|<table cellpadding=3 cellspacing=3>
				|<tr>
				|<td colspan=3 style="padding: 12px; border-radius: 15px; border-style: dotted; border-width: 2px;"><b><font size="6" color="#0000ff">$engine_name</font></b></td>
				|<td>depth $depth , nodes $nodesf M , nps $npsf kN/s , time $timef</td>
				|</tr>
				|<tr><td>
				|$pvs
				|</td></tr>
				|</table>
				""".stripMargin

				Builder.getwebe("enginetext").loadContent(content)
				
			}
		})
	}

	def engine_start()
	{
		engine.fen=commands.g.report_fen
		engine.go()

		enginelist.StartAll(commands.g)

		if(engine.engine_running)
		{
			selecttab("Engine")
		}
	}

	def engine_stop()
	{
		engine.stop()

		enginelist.StopAll

		Platform.runLater(new Runnable{def run{gb.clear_score}})
	}

	def engine_make()
	{

		val parts=engine.pvs.split(" ")
		var san=parts(0)

		val m=commands.g.b.sanToMove(san)

		if(m!=null)
		{
			click_move(m)

			commands.g.makeMove(m)

			update
		}

	}

	def engine_hint(dur: Int) {
		engine_start()
		try{Thread.sleep(dur)}catch{case e:Throwable=>{}}
		engine_stop()
		engine_make()
	}

	def load_engine()
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

			settings.set_variant_engine_path(path)

			val ecombo=Builder.getcombo("enginescombo")

			ecombo.addstore(path)

			Builder.setcvevall("engines",ecombo.items)

			load_current_engine

		}

	}

	def set_multipv(mpv:Int)
	{
		engine.set_multipv(mpv)

		enginelist.SetMultipv(mpv,commands.g)
	}

	// collect the ids of engine option update widgets
	// so that all engine options can be updated by handling their
	// apply button pressed events
	var allengineids=List[String]()

	def load_current_engine
	{
		val engine_path=settings.get_variant_engine_path()
		engine.load_engine(engine_path)

		if(engine.uci_engine_path=="")
		{
			println("warning: engine could not be loaded")
			return
		}

		if(engine_path.indexOf("atomkraft")>=0)
		{
			set_multipv(get_multipv)
			return
		}

		// uci

		engine.engine_intro=true

		engine.issue_command("uci\n")

		while(engine.uci_puff.indexOf("uciok")<0)
		{
			try{Thread.sleep(100)}catch{case e:Throwable=>{}}
		}

		var options=Map[String,Map[String,String]]()

		var option_list=List[String]()

		for(line<-engine.uci_puff.split("\r?\n").toList)
		{
			val parts=line.split(" ").toList
			var name=""
			val tokens=List("name","type","default","min","max")
			if(parts(0)=="option")
			{
				var i=1
				var buff=""
				var currentkind=""
				var option=Map[String,String]()
				var name=""
				while(i< parts.length)
				{
					val last=(i==(parts.length-1))
					if((tokens.contains(parts(i)))||last)
					{
						if(currentkind!="")
						{
							if(last)
							{
								if(buff!="") buff+=" "
								buff+=parts(i)
							}
							option+=(currentkind->buff)
							if(currentkind=="name")
							{
								name=buff
							}
							buff=""
						}
						currentkind=parts(i)
					}
					else
					{
						if(buff!="") buff+=" "
						buff+=parts(i)
					}
					i+=1
				}
				if(name!="")
				{
					options+=(name->option)
					option_list=option_list:+name
				}
			}
		}

		// construct xml file

		var widgets=""
		var r=1
		allengineids=List[String]()
		for(name<-option_list)
		{
			val option=options(name)
			if(option.contains("type"))
			{
				val kind=option("type")
				var widget=""
				val id=s"enginesettings#$engine_path#$name"
				allengineids=allengineids:+id
				val gr=s"""r="$r" c="2" """
				if(kind=="check")
				{
					widget=s"""
					|<checkbox $gr id="$id"/>
					""".stripMargin
				}
				if((kind=="spin")&&(option.contains("min"))&&(option.contains("max")))
				{
					var min=option("min")
					var max=option("max")
					val minv=data.Utils.parse[Int](min,0)
					val maxv=data.Utils.parse[Int](max,100)
					var span=maxv-minv
					if(minv==1) span+=1
					var unit=1
					if(span>10)
					{
						unit=span/10
					}
					widget=s"""
					|<slider width="300.0" $gr id="$id" min="$min" max="$max" majortickunit="$unit" showticklabels="true"/>
					""".stripMargin
				}
				if(kind=="button")
				{
					widget=s"""					
					|<hbox $gr>
					|<button text="$name" id="$id!apply"/>
					|<button text="$name and Close" id="$id!applyclose"/>
					|</hbox>
					""".stripMargin
				}
				if(kind=="string")
				{
					widget=s"""
					|<textfield $gr id="$id"/>
					""".stripMargin
				}
				if((widget!="")&&(name!="MultiPV"))
				{
					if(option.contains("default"))
					{
						if(Builder.getcval(id)==null)
						{
							Builder.setcval(id,option("default"))
						}
					}
					var value=Builder.getcvals(id,"")
					if(kind!="button")
					{
						if(kind=="spin")
						{
							value=""+data.Utils.parse[Double](value,0.0).toInt
						}
						val command=s"setoption name $name value $value"						
						engine.issue_command(command+"\n")
					}
					widgets+=s"""
					|<label r="$r" c="1" text="$name"/>
					|$widget
					""".stripMargin
					if(kind!="button")
					{
						/*widgets+=s"""
						|<button r="$r" c="3" id="$id!apply" text="Apply"/>
						|<button r="$r" c="4" id="$id!applyclose" text="Apply and Close"/>
						""".stripMargin*/
						widgets+=s"""
						|<button r="$r" c="4" id="$id!applyclose" text="Apply and Close"/>
						""".stripMargin
					}
					r+=1
				}
			}
		}
		var xml=s"""
		|<vbox padding="5">
		|<vbox padding="5" style="-fx-border-style: solid; -fx-border-width: 1px; -fx-border-radius: 10px;">
		|<scrollpane height="500.0" width="800.0">
		|<vbox padding="5" bimage="marble.jpg" cover="false">
		|<button id="applyallandclose" text="Apply All and Close"/>
		|<grid vgap="5" hgap="5">
		|$widgets
		|</grid>
		|</vbox>
		|</scrollpane>
		|</vbox>
		|</vbox>
		""".stripMargin
		writeStringToFile(new File("guidescriptors"+File.separator+"engineoptions.xml"),xml)

		// end uci

		set_multipv(get_multipv)
	}

	def get_selected_variant:String=Builder.getcombo("selectvariantcombo").get_selected

	def init_enginescombo
	{
		val ep=settings.get_variant_engine_path()
		val items=Builder.getcvevall("engines")

		val ecombo=Builder.getcombo("enginescombo")
		ecombo.create_from_list(items)
		ecombo.add(ep)
	}

	def variant_changed
	{
		update

		load_current_engine

		init_board_patterns

		init_enginescombo

		enginelist.Load

		enginelist.SetMultipv(get_multipv,commands.g)
	}

	def get_selected_engine:String=Builder.getcombo("enginescombo").get_selected

	def engine_selected
	{
		val running=engine.engine_running

		engine_stop

		val path=get_selected_engine

		settings.set_variant_engine_path(path)

		load_current_engine

		if(running) engine_start
	}

	def variant_selected(v:String=get_selected_variant)
	{		
		commands.exec(s"v $v")

		variant_changed
	}

	def getboardsize:Double=Builder.gsd("boardsize",400.0)

	def getboardcanvassize:Double=if(gb!=null) return gb.canvas_size.toDouble else 400.0

	case class BRect(x:Int=25, y:Int=25, width:Int=400, height:Int=400, ss:Int=50, hss:Int=25) {}

	def getbrect: BRect =
	{
		val x=Builder.gd("stages#recordrectdialog#x",25.0).toInt
		val y=Builder.gd("stages#recordrectdialog#y",25.0).toInt
		val width=Builder.gd("stages#recordrectdialog#width",400.0).toInt
		val height=Builder.gd("stages#recordrectdialog#height",400.0).toInt

		val ss=(width+height)/16
		val hss=ss/2

		BRect(x,y,width,height,ss,hss)
	}

	def click_square(sq:square.TSquare,flip:Boolean)
	{
		var rank=square.rankOf(sq)
		if(flip) rank=7-rank
		var file=square.fileOf(sq)
		if(flip) file=7-file

		val brect=getbrect

		val screenx:Int=brect.x+file*brect.ss+brect.hss
		val screeny:Int=brect.y+rank*brect.ss+brect.hss

		gui2.Robot.click_xy(screenx,screeny)
	}

	def click_move(m:move)
	{
		val doclick=Builder.gb("components#clickrect",false)

		if(!doclick) return

		var from=m.from
		var to=m.to

		click_square(m.from,settings.flip)
		try{Thread.sleep(50)}catch{case e:Throwable=>{}}
		click_square(m.to,settings.flip)
	}

	def learn_board_colors
	{
		val brect=getbrect
		val bim=gui2.Robot.getImage(brect.x, brect.y, brect.width, brect.height)

		val lxl=brect.hss+brect.ss*3
		val lyl=brect.hss+brect.ss*3

		val lxd=brect.hss+brect.ss*4
		val lyd=brect.hss+brect.ss*3

		val lcol=bim.getRGB(lxl,lyl)
		val dcol=bim.getRGB(lxd,lyd)

		Builder.setval("boardattrs#lcol",""+lcol)
		Builder.setval("boardattrs#dcol",""+dcol)
	}

	case class BAttrs(lcol:Int,dcol:Int) {}

	def getbattrs: BAttrs =
	{
		val lcol=Builder.gi("boardattrs#lcol",0)
		val dcol=Builder.gi("boardattrs#dcol",1)

		BAttrs(lcol,dcol)
	}

	var pstartpos=""
	var popenings=List[String]()
	var popeningsans=List[String]()

	def init_board_patterns
	{
		val b=new board
		b.reset
		pstartpos=b.report_pattern
		b.initMoveGen
		popenings=List[String]()
		while(b.nextLegalMove) {
			val bc=b.cclone
			bc.makeMove(b.current_move)
			popeningsans=popeningsans:+b.toSan(b.current_move)
			popenings=popenings:+bc.report_pattern
		}
	}

	def learn_board_pattern(flip:Boolean):String=
	{
		val brect=getbrect
		val bim=gui2.Robot.getImage(brect.x, brect.y, brect.width, brect.height)

		val battrs=getbattrs

		var buff=""
		var fbuff=""

		for(rank<- 0 to 7)
		{
			for(file<- 0 to 7)
			{
				var f=file
				var r=rank

				if(flip)
				{
					f=7-f
					r=7-r
				}

				val lx=brect.hss+brect.ss*f
				val ly=brect.hss+brect.ss*r

				var ecnt=0

				for(blurx<- -2 to 2)
				{
					for(blury<- -2 to 2)
					{
						val col=bim.getRGB(lx+blurx*brect.hss/10,ly+blury*brect.hss/10)
						if((col==battrs.lcol)||(col==battrs.dcol)) ecnt+=1
					}
				}

				buff+=(if(ecnt>20) "0" else "1")
			}
			buff+="\n"
		}
		buff
	}

	var learn_on=false

	def learn_thread_func
	{

		Platform.runLater(new Runnable{
			def run
			{				
				Builder.setbuttontext("learnboard","STOP")
			}
		})

		var found=false

		while(learn_on)
		{
			if(learn_board(false))
			{			
				found=true
				learn_on=false
			}
			else
			{				
				try{Thread.sleep(50)}catch{case e:Throwable=>{}}
			}
		}

		if(found)
		{
			Platform.runLater(new Runnable{
				def run
				{			
					learn_board(true)
				}
			})
		}

		Platform.runLater(new Runnable{
			def run
			{				
				Builder.setbuttontext("learnboard","LEARN")
			}
		})

	}

	def learn_board(make: Boolean): Boolean=
	{

		val buff=learn_board_pattern(false)
		val fbuff=learn_board_pattern(true)

		var found=false

		if(buff==pstartpos)
		{
			found=true

			if(make)
			{
				commands.exec("r")

				settings.flip=false

				update
			}
		}
		else
		{
			for(i<- 0 to (popenings.length-1))
			{
				val pc=popenings(i)
				
				if(fbuff==pc)
				{
					found=true

					if(make)
					{
						commands.exec("r")

						commands.exec("m "+popeningsans(i))

						settings.flip=true

						update
					}
				}
			}
		}

		if(!found)
		{
			val b=new board
			b.set_from_fen(commands.g.b.report_fen)
			b.initMoveGen
			while(b.nextLegalMove && !found) {
				val bc=b.cclone
				bc.makeMove(b.current_move)
				var newflip=false
				if(buff==bc.report_pattern)
				{
					found=true
				}
				if(fbuff==bc.report_pattern)
				{
					newflip=true
					
					found=true
				}
				if(found&&make)
				{
					val san=b.toSan(b.current_move)

					commands.exec("m "+san)

					settings.flip=newflip

					update
				}
			}
		}

		if(found&&make)
		{
			engine_hint(450)

			try{Thread.sleep(400)}catch{case e:Throwable=>{}}

			learn_start
		}

		found

	}

	def getdoclick = Builder.gb("components#clickrect",false)

	def manual_move_made(m:move,san:String)
	{
		commands.exec(s"m $san")

		update
	}

	def build_board
	{
		val boardvbox=Builder.getvboxn("boardvbox")

		if(boardvbox!=null)
		{

			gb=new guiboard((getboardsize/square.BOARD_SIZE).toInt)

			gb.manual_move_made_callback=manual_move_made

			boardvbox.getChildren().clear()

			boardvbox.getChildren().add(gb.rooth)

			val ms=Builder.stages("main").s
			ms.setHeight(getboardcanvassize+180.0)
			ms.setWidth(getboardcanvassize+getpanewidth+50.0)

			val paneheight=getboardcanvassize

			for(scrollpane<-scrollpanes)
			{				
				Builder.getcomp(scrollpane+"scrollpane").node.asInstanceOf[ScrollPane].setMinHeight(paneheight)
				Builder.getcomp(scrollpane+"scrollpane").node.asInstanceOf[ScrollPane].setMaxHeight(paneheight)
			}

			Builder.getcomp("colorpgntext").node.asInstanceOf[WebView].setMinHeight(paneheight)
			Builder.getcomp("colorpgntext").node.asInstanceOf[WebView].setMaxHeight(paneheight)

			Builder.getcomp("enginestext").node.asInstanceOf[WebView].setMinHeight(paneheight)
			Builder.getcomp("enginestext").node.asInstanceOf[WebView].setMaxHeight(paneheight)

			Builder.getcomp("enginegamestext").node.asInstanceOf[WebView].setMinHeight(paneheight)
			Builder.getcomp("enginegamestext").node.asInstanceOf[WebView].setMaxHeight(paneheight)

			val GAMES_STEP=((paneheight-70)/18).toInt

			for(gamebrowser<-List(pgn_game_browser,book_game_browser))
			{
				gamebrowser.sp.setMinHeight(paneheight)
				gamebrowser.sp.setMaxHeight(paneheight)

				gamebrowser.setStep(GAMES_STEP)
			}

			val buildcutoffvbox=Builder.getvboxn("cutbuildat")

			val select_cut_combo=new components.MyCombo(List("1","2","3","4","5","6","7","8","9","10","15","20","25","30","35","40","45","50","100","200"),"buildcutoff",(String)=>{})
			
			buildcutoffvbox.getChildren().add(select_cut_combo.n)

			update
		}
	}

	def update_book_text
	{
		val book_content=commands.g.book.currentPos.toPrintable(html=true)
        Builder.getwebe("booktext").loadContent(book_content)

        current_book_ecombo.c.create_from_list(butils.list_books())
		current_book_ecombo.c.select(settings.get_current_book(),set_trigger=false)

		commands.g.build_book_game_list
		book_game_browser.game_list=commands.g.book_games
        book_game_browser.update
	}

	def update
	{
		if(gb!=null)
		{

			val fen=commands.g.report_fen

			gb.set_from_fen(fen)

			gb.flip=settings.flip

			gb.draw_board

			gb.clear_highlight
			if(commands.g.current_node!=commands.g.root)
			{
				val dummy=new board
				dummy.set_from_fen(commands.g.current_node.parent.fen)
				val m=dummy.sanToMove(commands.g.current_node.genSan)
				if(m!=null)
				{
					val ram=dummy.toRealAlgebMove(m)
					gb.highlight_move(ram)
				}
			}

			Builder.getcomp("boardfenlabel").asInstanceOf[Builder.MyLabel].setText(fen)

			val cpc=commands.g.report_pgn_html(commands.g.current_node)

			Builder.setweb("colorpgntext",cpc)

			val mi=cpc.indexOf("padding: 3px")

			val len=cpc.length.toDouble

			var v=0.0

			if(mi>0) {
				v=(mi-2000)/len
			}

			val cpwe=Builder.getwebe("colorpgntext")

			cpwe.getLoadWorker().stateProperty().addListener(new ChangeListener[State]{
	            def changed(ov: ObservableValue[_ <: State], oldState: State, newState: State) {
	                if (newState == State.SUCCEEDED) {
	                	val h=cpwe.executeScript("document.body.scrollHeight").toString().toDouble
	                	var sh=h*v
	                	if(sh>h) sh=h
	                	if(sh< 0) sh=0
	                	cpwe.executeScript("window.scrollTo(" + 0 + ", " + sh + ")");
					}
				}
			})


			Builder.getcomp("pgntext").asInstanceOf[Builder.MyTextArea].setText(commands.g.report_pgn)

			Builder.getcomp("pgntreetext").asInstanceOf[Builder.MyTextArea].setText(commands.g.report_pgn_tree)

			Builder.setweb("movetext",commands.g.b.genPrintableMoveList(html=true))

			update_book_text

			if(engine.engine_running)
			{
				engine.stop()
				engine.fen=fen
				engine.go()
			}

			if(!engine.engine_running)
			{
				gb.clear_engine
				gb.clear_score
			}

			if(enginelist!=null)
			{
				enginelist.CheckRestartAll(commands.g)
			}

		}
	}

	def init_variantcombo_data
	{
		var i=0
		for(v<-settings.SUPPORTED_VARIANTS)
		{
			Builder.setcval(s"selectvariantcombo#items#$i",v)
			i+=1
		}

		Builder.setcval(s"selectvariantcombo#selected",settings.getvariant)
	}

	def game_browser_update()
	{
		update
	}

	def build_browsers
	{
		pgn_game_browser=new components.GameBrowser(selecttab,game_browser_update)
		Builder.getvbox("pgngamebrowservbox").addChild(pgn_game_browser.vb)

		book_game_browser=new components.GameBrowser(selecttab,game_browser_update)
		Builder.getvbox("bookgamebrowservbox").addChild(book_game_browser.vb)

		current_book_ecombo=new components.EditableCombo(butils.list_books(),"currentbook",scb,delb)
		Builder.getvbox("currentbookvbox").addChild(current_book_ecombo.n)
	}

	override def start(set_primaryStage: Stage)
	{
		gui2.Board.init_move_table

		Builder.eval_exp_callback=eval_exp

		Builder.startup

		commands.startup

		///////////////////////////////////////////////////

		init_variantcombo_data

		Builder.MyStage("main",set_stage=set_primaryStage,set_handler=handler,title="scalachessgui")

		build_browsers

		build_board

		enginelist=GEngineList(Builder.getwebe("enginestext"))

		///////////////////////////////////////////////////

		variant_changed
	}

	override def stop()
	{
		println("application stop method")

		print("shutting down command ... ")

		commands.shutdown

		println("done")

		print("shutting down builder ... ")

		Builder.shutdown

		println("done")

		print("shutting down engine games ... ")

		enginegames.ShutDown

		println("done")

		print("stopping engines ... ")

		engine.stop_engine_process

		enginelist.UnloadAll

		println("done")

		/*println("application stopped ok, will quit in 10 seconds")

		Thread.sleep(10000)*/
	}

}