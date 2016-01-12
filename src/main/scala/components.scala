package components

import javafx.application._
import javafx.stage._
import javafx.scene._
import javafx.scene.layout._
import javafx.scene.control._
import javafx.scene.canvas._
import javafx.scene.input._
import javafx.scene.paint._
import javafx.scene.text._
import javafx.event._
import javafx.geometry._
import javafx.scene.web._
import javafx.scene.image._
import javafx.collections._

import javafx.beans.value._

import java.io._

import collection.JavaConversions._
import scala.collection.mutable.ArrayBuffer
import org.apache.commons.io.FileUtils._

import settings._
import utils.Resource
import gui2.Engine
import gui.GuiClass
import commands.commands
import game.game
import builder._

object ButtonStyles
{
	val round=
		"-fx-background-color:#090a0c,linear-gradient(#38424b 0%, #1f2429 20%, #191d22 100%),linear-gradient(#20262b, #191d22),radial-gradient(center 50% 0%, radius 100%, rgba(114,131,148,0.9), rgba(255,255,255,0)); "+
		"-fx-background-radius:20,20,20,20; "+
		"-fx-text-fill:white; "+
		"-fx-effect:dropshadow( three-pass-box , rgba(0,0,0,0.6) , 5, 0.0 , 0 , 1 ); "+
		"-fx-text-fill:linear-gradient(white, #d0d0d0); "+
		"-fx-font-size:14px; "+
		"-fx-font-weight:bold; "+
		"-fx-padding:6 6 6 6;"
}

class MyButton( text: String , callback: () => Unit , style:String="" ) extends Button( text )
{
	setOnAction(new EventHandler[ActionEvent]{
		override def handle(e: ActionEvent)
		{
			callback()
		}
	});

	setStyle(style)
}

class ImageButton( text: String , imgpath: String , callback: () => Unit , style:String="" ) extends 
	Button(text,new ImageView(new Image(Resource.asStream(imgpath))))
{
	setOnAction(new EventHandler[ActionEvent]{
		override def handle(e: ActionEvent)
		{
			callback()
		}
	});

	setStyle(style)
}

case class Style(var sm:Map[String,String]=Map[String,String]())
{
	def asCss:String=
	{
		(for((k,v)<-sm if(v!="")) yield s"-fx-$k: $v").mkString("; ")
	}

	def add(k:String,v:String)
	{
		sm+=(k->v)
	}
}

class myTabPane(
	var width:Double=(-1.0),
	var height:Double=(-1.0)
	) extends TabPane()
{
	if(width>=0)
	{
		setMinWidth(width)
		setMaxWidth(width)
	}

	if(height>=0)
	{
		setMinHeight(height)
		setMaxHeight(height)
	}

	def add(
		caption:String="",
		content:Node=new Group(),
		closable:Boolean=false
		)
	{
		val tab=new Tab()

		tab.setText(caption)

		tab.setContent(content)

		tab.setClosable(closable)

		getTabs().add(tab)
	}
}

class myTextArea(
	var curoff:Boolean=true,
	var wrap:Boolean=false,
	var monospace:Boolean=false,
	var fontsize:Int=(-1),
	var width:Double=(-1.0),
	var height:Double=(-1.0)
	) extends TextArea()
{

	if(wrap) setWrapText(true)

	val s=Style()

	if(curoff) s.add("display-caret","false")

	if(monospace) s.add("font-family","monospace")
	if(fontsize>=0) s.add("font-size",s"$fontsize px")

	updStyle

	if(width>=0)
	{
		setMinWidth(width)
		setMaxWidth(width)
	}

	if(height>=0)
	{
		setMinHeight(height)
		setMaxHeight(height)
	}

	def updStyle
	{
		setStyle(s.asCss)
	}
}

class MyWebView(
	var width:Double=(-1.0),
	var height:Double=(-1.0),
	var doclick:Boolean=false,
	var pclick:Boolean=false
	)
{

	val w=new WebView()

	val e=w.getEngine()

	def set(content:String)
	{
		e.loadContent(content)
	}
	
	if(width>=0)
	{
		w.setMinWidth(width)
		w.setMaxWidth(width)
	}

	if(height>=0)
	{
		w.setMinHeight(height)
		w.setMaxHeight(height)
	}

	def default_click_callback(x:String)
	{
		println("clicked on "+x)
	}

	def default_pclick_callback(key:String,action:String,param:String)
	{
		println("clicked on "+key+" action "+action+" param "+param)
	}

	var click_callback:(String)=>Unit=default_click_callback
	var pclick_callback:(String,String,String)=>Unit=default_pclick_callback

	class myWebViewHandler extends EventHandler[MouseEvent]
	{
	
		def handle(mouseEvent:MouseEvent)
		{

			if(!doclick) return

			var x:Int=mouseEvent.getX().toInt
            var y:Int=mouseEvent.getY().toInt
            var etype:String=mouseEvent.getEventType().toString()

            if(etype=="MOUSE_CLICKED")
            {
            	if(!pclick)
            	{
            		click_callback(e.executeScript("x").toString())
            	}
            	else
            	{
            		pclick_callback(
            			e.executeScript("key").toString(),
            			e.executeScript("action").toString(),
            			e.executeScript("param").toString()
            		)
            	}
            }

        }

    }

    w.setOnMouseClicked(new myWebViewHandler())

}

class EditableCombo(set_items:List[String],key:String,callback:(String)=>Unit,delcallback:(String)=>Unit=null)
{
	var items=set_items

	val n=new HBox(5)

	val c=new MyCombo(items,key,callback)

	val t=new TextField()

	def add()
	{

		val item=t.getText()

		if(item!="")
		{
			if(items.indexOf(item)< 0)
			{
				items=items:+item
				c.create_from_list(items)
				c.select(item,set_trigger=true)
			}
			else
			{
				c.select(item,set_trigger=true)
			}
		}

		t.setText("")

	}

	val addb=new MyButton("Add",add)

	def del()
	{

		val selobj=c.c.getSelectionModel().getSelectedItem()

		if(selobj!=null)
		{
			val sel=selobj.toString()

			items=(for(item<-items  if(item!=sel)) yield item).toList

			c.create_from_list(items)

			delcallback(sel)
		}

	}

	val delb=new MyButton("Del",del)

	n.getChildren().addAll(c.n,t,addb)

	if(delcallback!=null)
	{
		n.getChildren().add(delb)
	}
}

class MyCombo(set_items:List[String],key:String,callback:(String)=>Unit)
{

	var items=set_items

	val n=new Group()

	var c:ComboBox[String]=null

	var trigger=true

	def create_from_list(set_items:List[String])
	{
		items=set_items

		n.getChildren().clear()

		c=new ComboBox[String]()

		c.getItems().addAll(FXCollections.observableList(items))

		n.getChildren.add(c)

		c.setOnAction(new myEventHandler())
	}

	def do_select(sel:String)
	{
		settings.set(key,sel)

		callback(sel)
	}

	def select(what:String,set_trigger:Boolean=false)
	{
		trigger=false

		val i=items.indexOf(what)

		if(i>=0)
		{
			c.getSelectionModel().select(i)
		}

		trigger=true

		if(set_trigger)
		{
			do_select(what)
		}
	}

	class myEventHandler() extends EventHandler[ActionEvent]
	{
		def handle(ev:ActionEvent)
		{
			//println("combo clicked")

			if(trigger)
			{
				val selobj=c.getSelectionModel().getSelectedItem()

				if(selobj!=null)
				{
					val sel=selobj.toString()

					do_select(sel)
				}
			}
		}
	}

	create_from_list(items)

	select(settings.get(key))
}

class MyStage(
	title:String="",
	x:Double=10.0,
	y:Double=10.0,
	width:Double=(-1.0),
	height:Double=(-1.0),
	modal:Boolean=false
	)
{

	val s=new Stage()

	s.setTitle(title)

	s.setX(x)
	s.setY(y)

	if(width>=0)
	{
		s.setMinWidth(width)
		s.setMaxWidth(width)
	}

	if(height>=0)
	{
		s.setMinHeight(height)
		s.setMaxHeight(height)
	}

	if(modal)
	{
		s.initModality(Modality.APPLICATION_MODAL)

		class MyWindowEventHandler extends EventHandler[WindowEvent]
		{
			def handle(ev:WindowEvent)
			{
				ev.consume()
			}
		}

		s.setOnCloseRequest(new MyWindowEventHandler())
	}

	def close
	{
		Platform.runLater(new Runnable
			{
				def run
				{
					s.close
				}
			}
		)
	}

}

class LogPane(val capacity:Int=100)
{
	val LOGPANE_WIDTH=800
	val LOGPANE_HEIGHT=500

	val w=new MyWebView(width=LOGPANE_WIDTH,height=LOGPANE_HEIGHT)

	val n=w.w

	val v=new VBox(10)

	var cnt=0

	var lines=Array[String]()

	def add(addline:String)
	{

		//println("log "+addline)

		if(lines.length>=capacity)
		{
			lines=lines.tail:+addline
		}
		else
		{
			if(lines.length>0)
			{
				lines=lines:+addline
			}
			else
			{
				lines=Array(addline)
			}
		}

		cnt+=1

		var i=cnt+1
		val items=(for(line<-lines.reverse) yield
		{
			i-=1
			s"""
				|<tr>
				|<td>
				|<b>$i.</b>
				|</td>
				|<td>
				|<font color="#00007f">$line</font>
				|</td>
				|</tr>
			""".stripMargin
		}).mkString("\n")

		val content=s"""
			|<table>
			|$items
			|</table>
		""".stripMargin

		Platform.runLater(new Runnable
			{
				def run
				{
					w.set(content)
				}
			}
		)

	}

}

class LogDialog(title:String,set_cancel_callback:()=>Unit=null,set_func:()=>Unit=null)
{

	val s=new MyStage(title=title,modal=true)

	val v=new VBox(10)

	/////////////////////////////////////////////////////

	var cancel_callback=set_cancel_callback

	def default_cancel_callback()
	{
		s.s.close()
	}

	if(cancel_callback==null) cancel_callback=default_cancel_callback

	/////////////////////////////////////////////////////

	var func=set_func

	def default_func()
	{

	}

	if(func==null) func=default_func

	/////////////////////////////////////////////////////

	val c=new MyButton("Cancel",cancel_callback)

	val lp=new LogPane()

	v.getChildren().addAll(
		c,
		lp.n
		)

	s.s.setScene(new Scene(v))

	def start
	{

		val th=new Thread(new Runnable
			{
				def run()
				{
					func()
				}
			}
		)

		th.start

		s.s.showAndWait()

	}

}

case class MyMenu(caption:String) extends Menu
{
	setText(caption)
}

case class MyMenuItem(caption:String,callback:()=>Unit) extends MenuItem
{
	setText(caption)

	setOnAction(new EventHandler[ActionEvent]{
		override def handle(e: ActionEvent)
		{
			callback()
		}
	});	
}

class MyEngine(update_callback:()=>Unit) extends Engine
{
	override def update_engine()
	{
		update_callback()
	}
}

class GameBrowser(selecttab:(String)=>Unit,updategui:()=>Unit)
{

	var game_list=List[String]()

	var step=20

	var from=0

	val vb=new VBox(5)

	def setStep(set_step:Int)
	{
		step=set_step
		update
	}

	def update
	{
		val len=game_list.length
		if(from>=len) from=len-step
		if(from< 0) from=0

		w.getEngine().loadContent(commands.g.printableGameList(from,from+step,true,game_list))
	}

	def tobegin()
	{
		from=0
		update
	}

	def back()
	{
		from-=step
		update
	}

	def fastback()
	{
		from-=10*step
		update
	}

	def fastforward()
	{
		from+=10*step
		update
	}

	def forward()
	{
		from+=step
		update
	}

	def toend()
	{
		from=game_list.length
		update
	}

	val hb=new HBox(5)

	hb.setPadding(new Insets(5,5,5,5))

	val sort_combo=new ComboBox[String]()

	sort_combo.getItems().addAll(FXCollections.observableList(List("White","WhiteElo","Black","BlackElo","Date","Event","Result")))

	def sort_selected(sort:Boolean)
	{
		val selobj=sort_combo.getSelectionModel().getSelectedItem()

		val sel=if(selobj==null) "" else selobj.toString()

		var games=(for(md5<-game_list) yield
		{
			val g=new game

			val v=settings.getvariant

			val path=s"games/$v/$md5.pgn"

			val pgn=org.apache.commons.io.FileUtils.readFileToString(
				new File(path),
				null.asInstanceOf[String]
			)

			g.parse_pgn(pgn,head_only=true)

			Tuple2[String,game](md5,g)
		}).toArray

		val search=sort_text.getText()
		sort_text.setText("")

		//println("search for _"+search+"_")

		def sortfunc(a:Tuple2[String,game],b:Tuple2[String,game]):Boolean=
		{
			if(sort) return a._2.get_header(sel)< b._2.get_header(sel)
			var matcha=false
			for((k,v)<-a._2.pgn_headers)
			{
				if(v.contains(search)) matcha=true
			}
			/*var matchb=false
			for((k,v)<-b._2.pgn_headers)
			{
				if(v.contains(search)) matchb=true
			}
			if(matcha && (!matchb)) return true*/
			matcha
		}

		games=games.sortWith(sortfunc)

		if(sel.contains("Elo")||(sel=="Result")) games=games.reverse

		game_list=(for(item<-games) yield item._1).toList

		from=0

		update

	}

	def do_selected_sort()
	{
		sort_selected(true)
	}

	def do_selected_search()
	{
		sort_selected(false)
	}

	sort_combo.setOnAction(new EventHandler[ActionEvent]
	{
		def handle(ev:ActionEvent)
		{			
			do_selected_sort
		}
	})

	val sort_text=new TextField()

	hb.getChildren().addAll(
		new MyButton("|<",tobegin),
		new MyButton("<<",fastback),
		new MyButton("<",back),

		new MyButton(">",forward),
		new MyButton(">>",fastforward),
		new MyButton(">|",toend),
		new Label("Sort by"),
		sort_combo,
		new MyButton("Sort",do_selected_sort),
		new Label("Search for: "),
		sort_text,
		new MyButton("Search",do_selected_search)
		)

	val sp=new ScrollPane()

	val width=builder.Builder.gsd("panewidth",750.0)

	sp.setMinWidth(width)
	sp.setMaxWidth(width)

	val w=new WebView()

	w.setMinWidth(2000)
	w.setMaxWidth(2000)
	w.setMinHeight(2000)
	w.setMaxHeight(2000)

	def handle()
	{
		val x=w.getEngine().executeScript("x")
		if(x!="")
		{
			commands.exec("gpgn "+x)
			selecttab("Color PGN")
			from=0
			update
		}
	}

	w.setOnMouseClicked(new EventHandler[MouseEvent]
	{			
		def handle(mouseEvent:MouseEvent)
		{

			var x:Int=mouseEvent.getX().toInt
            var y:Int=mouseEvent.getY().toInt
            var etype:String=mouseEvent.getEventType().toString()

            if(etype=="MOUSE_CLICKED")
            {
            	val x=w.getEngine().executeScript("x")

            	if(x!="")
            	{
            		commands.g.get_game(game_list,x.toString().toInt-1)
            		updategui()
            		selecttab("Color PGN")
            	}
            }

        }
    })

	sp.setContent(w)

	vb.getChildren().addAll(hb,sp)
}