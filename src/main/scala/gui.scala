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

////////////////////////////////////////////////////////////////////

import java.io._

import org.apache.commons.io.FileUtils._
import org.apache.commons.lang.time.DurationFormatUtils.formatDuration

////////////////////////////////////////////////////////////////////

import builder._
import guiboard._
import commands._
import board._
import movetable._
import square._
import move._
import settings._
import components._
import book._

import gui2.Engine

////////////////////////////////////////////////////////////////////

class GuiClass extends Application
{

	val scrollpanes=List("colorpgn","pgn","move","engine")

	def getpanewidth=Builder.gsd("panewidth",750.0)
	def getinnerpanewidth=getpanewidth-30.0

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
	}

	def handler(ev:MyEvent)
	{
		Builder.default_handler(ev)
		
		if(ev.kind=="checkbox changed")
		{
			if(ev.id=="bookenabled")
			{
				commands.g.pos_changed
				update
			}
		}

		if(ev.kind=="combo selected")
		{
			if(ev.id=="multipvcombo")
			{
				val running=engine.engine_running
				if(running) engine_stop
				set_multipv(get_multipv)
				if(running) engine_start
			}

			if(ev.id=="selectvariantcombo")
			{
				variant_selected()
			}
		}

		if(ev.kind=="webview clicked")
		{

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

	            		commands.g.tonode(gn)

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

			/*if(ev.id=="booktext")
			{

				val key=get_book_html.executeScript("key").toString()
				val action=get_book_html.executeScript("action").toString()
				val param=get_book_html.executeScript("param").toString()

				if(action=="annot")
				{
					exec(s"a $key $param")
					update_book_text
				}
				else if(action=="make")
				{
					exec(s"m $key")
					update
				}
				else if(action=="del")
				{
					exec(s"del $key")
					update
				}

			}*/
		}

		if(ev.kind=="menuitem clicked")
		{

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
			if(ev.id=="boardprofile")
			{
				build_board
			}
		}

		if(ev.kind=="button pressed")
		{

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

			if(ev.id=="boardcontrolpanelstart")
			{
				engine_start
			}

			if(ev.id=="boardcontrolpanelstop")
			{
				engine_stop
			}

			if(ev.id=="boardcontrolpanelmake")
			{
				engine_make
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

			if((ev.id=="editpgnok")||(ev.id=="editpgndel"))
			{
				val field=Builder.gettext("pgnfieldname").getText

				if(field!="")
				{
					if(ev.id=="editpgndel")
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


			if(ev.id=="boardcontrolpanelflip")
			{
				settings.flip=(!settings.flip)

				update
			}

			if(ev.id=="boardcontrolpanelreset")
			{
				commands.exec("r")

				update
			}

			if(ev.id=="boardcontrolpaneldel")
			{
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

	val engine=new components.MyEngine(update_engine)

	def get_multipv:Int=
	{
		Builder.gi("components#multipvcombo#selected",3)
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

		selecttab("Engine")
	}

	def engine_stop()
	{
		engine.stop()
		Platform.runLater(new Runnable{def run{gb.clear_score}})
	}

	def engine_make()
	{

		val parts=engine.pvs.split(" ")
		var san=parts(0)

		val m=commands.g.b.sanToMove(san)

		if(m!=null)
		{
			commands.g.makeMove(m)

			update
		}

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

			load_current_engine

		}

	}

	def set_multipv(mpv:Int)
	{
		engine.set_multipv(mpv)
	}

	def load_current_engine
	{
		val engine_path=settings.get_variant_engine_path()
		engine.load_engine(engine_path)
		set_multipv(get_multipv)
	}

	def get_selected_variant:String=Builder.getcombo("selectvariantcombo").get_selected

	def variant_selected(v:String=get_selected_variant)
	{
		val running=engine.engine_running
		
		if(running)
		{
			Builder.setcval("message","Variant cannot be changed while engine is running.")
			Builder.MyStage("infodialog",modal=true,set_handler=handler,title="Warning")
			return
		}

		movetable.init(v)
		
		commands.exec(s"v $v")

		update

		load_current_engine
	}

	def getboardsize:Double=Builder.gsd("boardsize",400.0)

	def getboardcanvassize:Double=if(gb!=null) return gb.canvas_size.toDouble else 400.0

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

			Builder.setweb("colorpgntext",commands.g.report_pgn_html(commands.g.current_node))

			Builder.getcomp("pgntext").asInstanceOf[Builder.MyTextArea].setText(commands.g.report_pgn)

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

		settings.load

		///////////////////////////////////////////////////

		init_variantcombo_data

		Builder.MyStage("main",set_stage=set_primaryStage,set_handler=handler,title="scalachessgui")

		build_browsers

		///////////////////////////////////////////////////

		variant_selected()

		commands.startup

		build_board
	}

	override def stop()
	{
		commands.shutdown

		Builder.shutdown

		engine.stop_engine_process
	}

}