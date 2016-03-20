package builder

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
import javafx.collections._

import collection.JavaConversions._

////////////////////////////////////////////////////////////////////

import java.io._
import scala.io._

////////////////////////////////////////////////////////////////////

import data._

////////////////////////////////////////////////////////////////////

case class MyEvent(
		val id:String="",
		val kind:String="",
		val value:String="",
		val x:Int=0,
		val y:Int=0
	)
{

}

object ClipboardSimple extends java.awt.datatransfer.ClipboardOwner
{

	override def lostOwnership(aClipboard:java.awt.datatransfer.Clipboard,aContents:java.awt.datatransfer.Transferable)
	{
		//do nothing
	}

	def clipget:String=getClipboardContents

	def getClipboardContents:String=
	{
		var result=""

		val clipboard = java.awt.Toolkit.getDefaultToolkit().getSystemClipboard()

		val contents = clipboard.getContents(null)

		val hasTransferableText =
			(contents != null) &&
			contents.isDataFlavorSupported(java.awt.datatransfer.DataFlavor.stringFlavor)

		if(hasTransferableText)
		{
			try
			{
				result = contents.getTransferData(java.awt.datatransfer.DataFlavor.stringFlavor).toString
			}
			catch
			{
				case _ : Throwable => result=""
			}
		}

		result
	}

	def clipset(content:String)
	{
		setClipboardContents(content)
	}

	def setClipboardContents(content:String)
	{
		val stringSelection = new java.awt.datatransfer.StringSelection(content)

		val clipboard = java.awt.Toolkit.getDefaultToolkit().getSystemClipboard()

		clipboard.setContents(stringSelection, this)
	}

}

object GuiUtils
{
	def hexToColor(hex:String):Color=
	{
		val red=java.lang.Integer.valueOf(hex.substring(1,3),16)
		val green=java.lang.Integer.valueOf(hex.substring(3,5),16)
		val blue=java.lang.Integer.valueOf(hex.substring(5,7),16)

		Color.rgb(red,green,blue)
	}

	def colorToHex(col:Color):String=
	{
		val red=(col.getRed()*255).toInt
		val green=(col.getGreen()*255).toInt
		val blue=(col.getBlue()*255).toInt

		"#%02X%02X%02X".format(red,green,blue)
	}
}

object Resource
{
	def asStream(path:String):InputStream=
	{
		getClass().getClassLoader().getResourceAsStream(path)
	}

	def asSource(path:String):Source=
	{
		Source.fromInputStream(asStream(path))
	}

	def asString(path:String):String=
	{
		asSource(path).mkString
	}
}

object Builder
{

	var gpath="guidescriptors"

	def dpath(name:String):String=s"$gpath/$name.xml"

	var values:Data=null

	var components=Map[String,MyComponent]()

	var stages=Map[String,MyStage]()

	/////////////////////////////////////////

	val PredefStyles=Map(
		"round"->
			("-fx-background-color:#090a0c,linear-gradient(#38424b 0%, #1f2429 20%, #191d22 100%),linear-gradient(#20262b, #191d22),radial-gradient(center 50% 0%, radius 100%, rgba(114,131,148,0.9), rgba(255,255,255,0)); "+
			"-fx-background-radius:20,20,20,20; "+
			"-fx-text-fill:white; "+
			"-fx-effect:dropshadow( three-pass-box , rgba(0,0,0,0.6) , 5, 0.0 , 0 , 1 ); "+
			"-fx-text-fill:linear-gradient(white, #d0d0d0); "+
			"-fx-font-size:14px; "+
			"-fx-font-weight:bold; "+
			"-fx-padding:6 6 6 6;"),
		"monotext"->
			"-fx-font-family: monospace; -fx-display-caret: false;"
	)

	val SCROLLBARPOLICIES=Map[String,ScrollPane.ScrollBarPolicy](
			"asneeded"->ScrollPane.ScrollBarPolicy.AS_NEEDED,
			"never"->ScrollPane.ScrollBarPolicy.NEVER,
			"always"->ScrollPane.ScrollBarPolicy.ALWAYS
		)

	/////////////////////////////////////////

	type THandler=(MyEvent)=>Unit

	def default_handler(ev:MyEvent)
	{
		val id=ev.id
		val kind=ev.kind
		val value=ev.value

		//println(s"event id = $id , kind = $kind , value = $value")
	}

	def startup
	{
		values=Data.loadXML(dpath("values"))

		values=Data.loadPatchXML(values,dpath("defaults"))
		if(new File("guidescriptors"+File.separator+"personal.xml").exists())
		{
			values=Data.loadPatchXML(values,dpath("personal"))
		}
		else
		{
			println("personal settings missing")
		}
	}

	def shutdown
	{
		Data.saveXML(values,dpath("values"))
		Data.saveXMLPretty(values,dpath("valuespretty"))
	}

	def cpath(id:String)=s"components#$id"
	def spath(id:String)=s"settings#$id"
	def ppath(profileid:String,name:String,id:String)=s"profiles#$profileid#$name#$id"

	def getcomp(id:String):MyComponent=
	{
		if(components.contains(id)) return components(id)
		null
	}

	def getvbox(id:String):MyVBox=
	{
		val comp=getcomp(id)
		if((comp!=null)&&(comp.isInstanceOf[MyVBox])) return comp.asInstanceOf[MyVBox]
		null
	}

	def getvboxn(id:String):VBox=
	{
		val vbox=getvbox(id)
		if(vbox==null) return null
		vbox.node.asInstanceOf[VBox]
	}

	def getcombo(id:String):MyCombo=
	{
		val comp=getcomp(id)
		if((comp!=null)&&(comp.isInstanceOf[MyCombo])) return comp.asInstanceOf[MyCombo]
		null
	}

	def getlabel(id:String):MyLabel=
	{
		val comp=getcomp(id)
		if((comp!=null)&&(comp.isInstanceOf[MyLabel])) return comp.asInstanceOf[MyLabel]
		null
	}

	def gettext(id:String):MyTextField=
	{
		val comp=getcomp(id)
		if((comp!=null)&&(comp.isInstanceOf[MyTextField])) return comp.asInstanceOf[MyTextField]
		null
	}

	def setbuttontext(id:String, what:String)=
	{
		val comp=getcomp(id)
		if((comp!=null)&&(comp.isInstanceOf[MyButton]))
		{
			comp.node.asInstanceOf[Button].setText(what)
		}
	}

	def gettextarea(id:String):MyTextArea=
	{
		val comp=getcomp(id)
		if((comp!=null)&&(comp.isInstanceOf[MyTextArea])) return comp.asInstanceOf[MyTextArea]
		null
	}

	def getweb(id:String):WebView=
	{
		val comp=getcomp(id)
		if((comp!=null)&&(comp.isInstanceOf[MyWebView])) return comp.node.asInstanceOf[WebView]
		null
	}

	def getwebe(id:String):WebEngine=
	{
		val web=getweb(id)
		if(web==null) return null
		web.getEngine()
	}

	def setweb(id:String,content:String)
	{
		val web=getweb(id)
		if(web!=null)
		{
			web.getEngine().loadContent(content)
		}
	}

	def getval(path:String):Data=
	{
		Data.get(values,path)
	}

	def default_eval_exp(exp:String):String=exp

	var eval_exp_callback:(String)=>String=default_eval_exp

	def gs(path:String,default:String=""):String=
	{
		val d=getval(path)

		if(d==null) return default

		if(d.isInstanceOf[StringData])
		{
			val v=d.asInstanceOf[StringData].value

			return v
		}

		default
	}

	def gss(path:String,default:String=""):String=gs(spath(path),default)

	def gi(path:String,default:Int):Int=
	{
		val d=getval(path)

		Utils.parse[Int](gs(path,""),default)
	}

	def gsi(path:String,default:Int=0):Int=gi(spath(path),default)

	def gd(path:String,default:Double):Double=
	{
		val d=getval(path)

		Utils.parse[Double](gs(path,""),default)
	}

	def gsd(path:String,default:Double=0.0):Double=gd(spath(path),default)

	def gb(path:String,default:Boolean):Boolean=
	{
		val d=getval(path)

		Utils.parse[Boolean](gs(path,""),default)
	}

	def gsb(path:String,default:Boolean=false):Boolean=gb(spath(path),default)

	def getcval(id:String):Data=
	{
		Data.get(values,cpath(id))
	}

	def getpval(profileid:String,name:String,id:String):Data=
	{
		Data.get(values,ppath(profileid,name,id))
	}

	def gps(profileid:String,name:String,id:String,default:String=""):String=
	{
		gs(ppath(profileid,name,id),default)
	}

	def getsval(id:String):Data=
	{
		Data.get(values,spath(id))
	}

	def getcvals(id:String,default:String=""):String=
	{
		Data.gs(values,cpath(id),default)
	}

	def getcvall(id:String):List[String]=
	{
		val listdataany=Data.get(values,cpath(id))

		if(listdataany==null) return List[String]()

		val listdata=listdataany.asInstanceOf[SeqData]

		Data.toList(listdata)
	}

	def setval(path:String,what:String)
	{
		values=Data.set(values,path,StringData(what))
	}

	def setval(path:String,what:Data)
	{
		values=Data.set(values,path,what)
	}

	def setcval(id:String,what:String)
	{
		setval(cpath(id),what)
	}

	def setcval(id:String,what:Data)
	{
		setval(cpath(id),what)
	}

	def setsval(id:String,what:Data)
	{
		setval(spath(id),what)
	}

	def setsval(id:String,what:String)
	{
		setval(spath(id),StringData(what))
	}

	def setsval_safe(id:String,what:Data)
	{
		if((id=="")||(what==null)) return

		setval(spath(id),what)
	}

	def setpval(profileid:String,name:String,id:String,what:Data)
	{
		setval(ppath(profileid,name,id),what)
	}

	def setpval_safe(profileid:String,name:String,id:String,what:Data)
	{
		if((profileid=="")||(name=="")||(id=="")||(what==null)) return

		setval(ppath(profileid,name,id),what)
	}

	def setcval_safe(id:String,what:String)
	{
		if((id=="")||(what==null)) return

		setval(cpath(id),what)
	}

	def setcval_safe(id:String,what:Data)
	{
		if((id=="")||(what==null)) return

		setval(cpath(id),what)
	}

	case class MyStage(
		id:String,
		set_stage:Stage=new Stage(),
		set_handler:THandler=default_handler,
		do_show:Boolean=true,
		do_build:Boolean=true,
		do_size:Boolean=true,
		unclosable:Boolean=false,
		modal:Boolean=false,
		title:String=null
	)
	{
		val s=set_stage

		s.setTitle(
				if(title!=null) title else Data.gs(values,s"stages#$id#title","")
			)

		s.setX(Data.gd(values,s"stages#$id#x",10.0))
		s.setY(Data.gd(values,s"stages#$id#y",10.0))
		
		if(do_size)
		{
			s.setWidth(Data.gd(values,s"stages#$id#width",600.0))
			s.setHeight(Data.gd(values,s"stages#$id#height",400.0))
		}

		s.xProperty().addListener(
			new ChangeListener[Number]
			{
				def changed(ov:ObservableValue[_ <: Number],old_val:Number,new_val:Number)
				{ 
					setval(s"stages#$id#x", new_val.toString())
				}
			}
		)

		s.yProperty().addListener(
			new ChangeListener[Number]
			{
				def changed(ov:ObservableValue[_ <: Number],old_val:Number,new_val:Number)
				{ 
					setval(s"stages#$id#y", new_val.toString())
				}
			}
		)

		s.widthProperty().addListener(
			new ChangeListener[Number]
			{
				def changed(ov:ObservableValue[_ <: Number],old_val:Number,new_val:Number)
				{ 
					setval(s"stages#$id#width", new_val.toString())
				}
			}
		)

		s.heightProperty().addListener(
			new ChangeListener[Number]
			{
				def changed(ov:ObservableValue[_ <: Number],old_val:Number,new_val:Number)
				{ 
					setval(s"stages#$id#height", new_val.toString())
				}
			}
		)

		if(do_build)
		{
			val p=build(dpath(id),set_handler)

			s.setScene(new Scene(p))
		}

		if(modal)
		{
			s.initModality(Modality.APPLICATION_MODAL)
		}

		if(unclosable)
		{
			s.setOnCloseRequest(new EventHandler[WindowEvent]
			{
				def handle(ev:WindowEvent)
				{
					ev.consume()
				}
			})
		}

		stages+=(id->this)

		if(do_show) s.show()

		def setscene(p:Parent)
		{
			s.setScene(new Scene(p))
		}

		def close
		{
			s.close()
		}

		def show
		{
			s.show()
		}

		def setWidth(w:Double)
		{
			s.setWidth(w)
		}

		def setHeight(h:Double)
		{
			s.setHeight(h)
		}
	}

	def closeStage(id:String)
	{
		if(stages.contains(id))
		{
			val stage=stages(id)

			stage.close
		}
	}

	var profiles=Map[String,Profile]()

	def get_profile(id:String):Profile=
	{
		if(profiles.contains(id)) return profiles(id)
		val p=new Profile
		p.id=id
		profiles+=(id->p)
		p
	}

	class Profile
	{
		var id=""
		var elements=Map[String,MyComponent]()

		var combo:MyCombo=null
		var nametext:MyTextField=null

		var handler:THandler=null

		def apply
		{
			val name=combo.get_selected

			for((eid,element)<-elements)
			{
				val gval=element.get_gui_value

				setsval_safe(eid,gval)

				if((name!="")&&(eid!=combo.id)) setpval_safe(id,name,eid,gval)
			}

			if(handler!=null)
			{
				handler(MyEvent(id,"profile applied",""))
			}
		}

		def add
		{
			val name=nametext.getText

			if(name=="") return

			combo.add(name)

			nametext.setText("")

			apply
		}

		def del
		{
			val name=combo.get_selected

			if(name!="")
			{
				val pval=getval(s"profiles#$id").asInstanceOf[MapData]

				if(pval!=null)
				{
					if(pval.map.contains(name))
					{
						val modmap=pval.map-name

						val modpval=MapData(modmap)

						setval(s"profiles#$id",modpval)
					}
				}

				combo.del(name)

				nametext.setText("")
			}
		}

		def select(sel:String)
		{
			if(sel!="")
			{
				for((eid,element)<-elements if(eid!=combo.id))
				{
					val gval=getpval(id,sel,eid)

					element.set_gui_value_safe(gval)
				}

				apply
			}
		}
	}

	abstract class MyComponent
	{

		/////////////////////////////////////

		var tag:Tag=null
		var id=""
		var profile=""
		var action=""
		var properties=Map[String,String]()
		var parent:MyComponent=null
		var children=Seq[MyComponent]()
		var handler:THandler=default_handler

		/////////////////////////////////////

		var node:Node=null
		var menu_node:Menu=null
		var menu_item_node:MenuItem=null
		var trigger:Boolean=true

		/////////////////////////////////////

		def gs(name:String,default:String=null):String=
		{
			if(properties.contains(name))
			{
				val v=properties(name)

				if(v.length>1)
				{
					if(v(0)=='$')
					{
						val exp=v.substring(1)

						return eval_exp_callback(exp)
					}
				}

				return v
			}

			default
		}

		def gi(name:String,default:Int=0):Int=
		{
			Utils.parse[Int](gs(name),default)
		}

		def gd(name:String,default:Double=0.0):Double=
		{
			Utils.parse[Double](gs(name),default)
		}

		def gb(name:String,default:Boolean=false):Boolean=
		{
			Utils.parse[Boolean](gs(name),default)
		}

		/////////////////////////////////////

		def get_gui_value:Data=StringData("")

		def set_gui_value(d:Data){}

		def set_gui_value_safe(d:Data,set_trigger:Boolean=false)
		{
			if(d==null) return

			trigger=set_trigger
			set_gui_value(d)
			trigger=true
		}

		/////////////////////////////////////

		def build

		/////////////////////////////////////

		def build_common
		{
			var style=gs("style","")

			if(node!=null)
			{
				if(PredefStyles.contains(style))
				{
					node.setStyle(PredefStyles(style))
				}
				else
				{
					node.setStyle(style)
				}
			}

			if(node.isInstanceOf[Region])
			{
				if(properties.contains("width"))
				{
					val width=gd("width",0.0)
					node.asInstanceOf[Region].setMinWidth(width)
					node.asInstanceOf[Region].setMaxWidth(width)
				}

				if(properties.contains("height"))
				{
					val height=gd("height",0.0)
					node.asInstanceOf[Region].setMinHeight(height)
					node.asInstanceOf[Region].setMaxHeight(height)
				}

				if(properties.contains("padding"))
				{
					val pad=gd("padding",0.0)
					node.asInstanceOf[Region].setPadding(new Insets(pad,pad,pad,pad))
				}

				if(properties.contains("bimage"))
				{
					val path="images/"+gs("bimage","")

					val contain=gb("contain",false)
					val cover=gb("cover",true)

					node.asInstanceOf[Region].setBackground(new Background(new BackgroundImage(
						new Image(Resource.asStream(path)),
						BackgroundRepeat.REPEAT,BackgroundRepeat.REPEAT,BackgroundPosition.CENTER,
						new BackgroundSize(BackgroundSize.AUTO,BackgroundSize.AUTO,false,false,contain,cover)
					)))
				}
			}
		}

		def build_all
		{

			/////////////////////////////////////

			build

			build_common

			/////////////////////////////////////

			if((profile!="")&&(id!=""))
			{
				val p=get_profile(profile)

				p.handler=handler

				var do_copy=false

				if(action=="select")
				{
					p.combo=this.asInstanceOf[MyCombo]
					p.elements+=(id->this)
					do_copy=true
				}
				else if(action=="name")
				{
					p.nametext=this.asInstanceOf[MyTextField]
				}
				else if(action=="")
				{
					p.elements+=(id->this)
					do_copy=true
				}

				if(do_copy)
				{
					val sval=getsval(id)

					setcval_safe(id,sval)
				}
			}

			if(id!="")
			{
				val cval=getcval(id)

				set_gui_value_safe(cval)
			}

			/////////////////////////////////////

		}

		/////////////////////////////////////

	}

	class MyMenuBar extends MyComponent
	{
		override def build
		{
			node=new MenuBar()
		}
	}

	class MyMenu extends MyComponent
	{
		override def build
		{
			menu_node=new Menu()

			menu_node.setText(gs("text"))
		}
	}

	class MyMenuItem extends MyComponent
	{
		override def build
		{
			menu_item_node=new MenuItem()

			menu_item_node.setText(gs("text"))
		}
	}

	class MyVBox extends MyComponent
	{
		override def build
		{
			node=new VBox(gd("gap"))
		}

		def addChild(n:Node)
		{
			node.asInstanceOf[VBox].getChildren().add(n)
		}
	}

	class MyHBox extends MyComponent
	{
		override def build
		{
			node=new HBox(gd("gap"))
		}
	}

	class MyLabel extends MyComponent
	{
		override def build
		{
			node=new Label(gs("text"))
		}

		def setText(what:String)
		{
			node.asInstanceOf[Label].setText(what)
		}

		override def set_gui_value(set_value:Data)
		{			
			node.asInstanceOf[Label].setText(Utils.parsestr(set_value.asInstanceOf[StringData].value,""))
		}
	}

	class MyCombo extends MyComponent
	{
		var c:ComboBox[String]=null

		var items=List[String]()

		override def build
		{
			node=new Group()

			val set_items=getcvall(s"$id#items")

			val sel=getcvals(s"$id#selected")

			create_from_list(set_items)

			select(sel)
		}

		override def get_gui_value:Data=
		{
			var sel=""

			val selobj=c.getSelectionModel().getSelectedItem()

			if(selobj!=null)
			{
				sel=selobj.toString()
			}

			val seldata=StringData(sel)

			val itemsdata=Data.fromList(items)

			MapData(Map("selected"->seldata,"items"->itemsdata))
		}

		override def set_gui_value(set_value:Data)
		{
			val mapdata=set_value.asInstanceOf[MapData]

			var set_sel=""

			if(mapdata.map.contains("selected"))
			{
				val seldata=mapdata.map("selected").asInstanceOf[StringData]

				set_sel=seldata.value
			}

			var set_items=List[String]()

			if(mapdata.map.contains("items"))
			{
				val itemsdata=mapdata.map("items").asInstanceOf[SeqData]

				set_items=Data.toList(itemsdata)
			}

			create_from_list(set_items)

			select(set_sel)
		}

		def get_selected:String=
		{
			val guidata=get_gui_value.asInstanceOf[MapData]

			guidata.map("selected").asInstanceOf[StringData].value
		}

		def select(what:String,set_trigger:Boolean=false)
		{
			val i=items.indexOf(what)

			trigger=set_trigger

			if(i>=0)
			{
				c.getSelectionModel().select(i)
			}

			trigger=true
		}

		def create_from_list(set_items:List[String])
		{
			items=set_items

			node.asInstanceOf[Group].getChildren().clear()

			c=new ComboBox[String]()

			c.getItems().addAll(FXCollections.observableList(items))

			node.asInstanceOf[Group].getChildren.add(c)

			c.setOnAction(new EventHandler[ActionEvent]
			{
				def handle(ev:ActionEvent)
				{
					if(trigger)
					{
						val sel=get_selected

						if(sel!="")
						{
							setcval_safe(s"$id#selected",sel)

							if((profile!="")&&(action=="select"))
							{
								get_profile(profile).select(sel)
							}

							handler(MyEvent(id,"combo selected",sel))
						}
					}
				}
			})
		}

		def add(what:String)
		{
			if(what=="") return

			if(!items.contains(what))
			{
				create_from_list(items:+what)
			}

			select(what)
		}

		def del(what:String)
		{
			if(what=="") return

			if(items.contains(what))
			{				
				create_from_list((for(item<-items if(item!=what)) yield item).toList)
			}
		}
	}

	class MyButton extends MyComponent
	{
		override def build
		{
			val text=gs("text")
			val img=gs("img")

			if(img!=null)
			{
				node=new Button(text,new ImageView(new Image(Resource.asStream(img))))
			}
			else
			{
				node=new Button(text)
			}

			node.asInstanceOf[Button].setOnAction(new EventHandler[ActionEvent]
			{
				def handle(ev:ActionEvent)
				{
					if(profile!="")
					{
						if(action=="apply")
						{
							get_profile(profile).apply
						}

						if(action=="add")
						{
							get_profile(profile).add
						}

						if(action=="del")
						{
							get_profile(profile).del
						}
					}

					handler(MyEvent(id,"button pressed",""))
				}
			})
		}
	}

	class MyColorPicker extends MyComponent
	{
		override def build
		{
			node=new ColorPicker()

			node.asInstanceOf[ColorPicker].setOnAction(new EventHandler[ActionEvent]
			{
				def handle(ev:ActionEvent)
				{
					if(trigger)
					{
						val col=node.asInstanceOf[ColorPicker].getValue()

						val val_str=GuiUtils.colorToHex(col)

						setcval_safe(id,val_str)

						handler(MyEvent(id,"color picked",val_str))
					}
				}
			})
		}

		override def get_gui_value:Data=
		{
			StringData(GuiUtils.colorToHex(node.asInstanceOf[ColorPicker].getValue()))
		}

		override def set_gui_value(set_value:Data)
		{
			node.asInstanceOf[ColorPicker].setValue(
				GuiUtils.hexToColor(Utils.parsestr(set_value.asInstanceOf[StringData].value,"#ffffff")))
		}
	}

	class MyGridPane extends MyComponent
	{
		override def build
		{
			node=new GridPane()

			if(properties.contains("hgap"))
			{
				val hgap=gi("hgap",0)

				node.asInstanceOf[GridPane].setHgap(hgap)
			}

			if(properties.contains("vgap"))
			{
				val vgap=gi("vgap",0)

				node.asInstanceOf[GridPane].setVgap(vgap)
			}
		}
	}

	class MySlider extends MyComponent
	{
		override def build
		{
			node=new Slider()

			trigger=false

			if(properties.contains("min"))
			{
				node.asInstanceOf[Slider].setMin(gd("min",0.0))
			}
			if(properties.contains("max"))
			{
				node.asInstanceOf[Slider].setMax(gd("max",0.0))
			}
			node.asInstanceOf[Slider].setValue((node.asInstanceOf[Slider].getMax()-node.asInstanceOf[Slider].getMin())/2)
			if(properties.contains("showticklabels"))
			{
				node.asInstanceOf[Slider].setShowTickLabels(gb("showticklabels",false))
			}
			if(properties.contains("showtickmarks"))
			{
				node.asInstanceOf[Slider].setShowTickMarks(gb("showtickmarks",false))
			}
			if(properties.contains("majortickunit"))
			{
				node.asInstanceOf[Slider].setMajorTickUnit(gd("majortickunit",1.0))
			}
			if(properties.contains("minortickcount"))
			{
				node.asInstanceOf[Slider].setMinorTickCount(gi("minortickcount",1))
			}
			if(properties.contains("blockincrement"))
			{
				node.asInstanceOf[Slider].setBlockIncrement(gd("blockincrement",1.0))
			}

			trigger=true

			node.asInstanceOf[Slider].valueProperty().addListener(new ChangeListener[Number]
			{
				def changed(ov:ObservableValue[_ <: Number],old_val:Number,new_val:Number)
				{
					if(trigger)
					{
						val new_val_str=new_val.toString()					

						setcval_safe(id,new_val_str)

						handler(MyEvent(id,"slider changed",new_val_str))
					}
				}
			})
		}

		override def get_gui_value:Data=
		{
			StringData(""+node.asInstanceOf[Slider].getValue())
		}

		override def set_gui_value(set_value:Data)
		{			
			node.asInstanceOf[Slider].setValue(Utils.parse[Double](set_value.asInstanceOf[StringData].value,0.0))
		}
	}

	class MyTextField extends MyComponent
	{
		override def build
		{
			node=new TextField(gs("text"))
		}

		override def get_gui_value:Data=
		{
			StringData(node.asInstanceOf[TextField].getText())
		}

		override def set_gui_value(set_value:Data)
		{			
			node.asInstanceOf[TextField].setText(Utils.parsestr(set_value.asInstanceOf[StringData].value,""))
		}

		def getText=node.asInstanceOf[TextField].getText()

		def setText(what:String)=node.asInstanceOf[TextField].setText(what)
	}

	class MyTextArea extends MyComponent
	{
		override def build
		{
			node=new TextArea(gs("text"))

			if(properties.contains("wrap"))
			{
				node.asInstanceOf[TextArea].setWrapText(gb("wrap",true))
			}
		}

		override def get_gui_value:Data=
		{
			StringData(node.asInstanceOf[TextArea].getText())
		}

		override def set_gui_value(set_value:Data)
		{			
			node.asInstanceOf[TextArea].setText(Utils.parsestr(set_value.asInstanceOf[StringData].value,""))
		}

		def setText(what:String)
		{
			node.asInstanceOf[TextArea].setText(what)
		}

		def getText:String=
		{
			node.asInstanceOf[TextArea].getText()
		}
	}

	class MyCheckBox extends MyComponent
	{
		override def build
		{
			node=new CheckBox()

			node.asInstanceOf[CheckBox].selectedProperty().addListener(new ChangeListener[java.lang.Boolean]
			{
				def changed(ov:ObservableValue[_ <: java.lang.Boolean],old_val:java.lang.Boolean,new_val:java.lang.Boolean)
				{
					if(trigger)
					{
						setcval_safe(id,""+new_val)

						handler(MyEvent(id,"checkbox changed",""+new_val))
					}
				}
			})
			}

		override def get_gui_value:Data=
		{
			StringData(""+Utils.parse[Boolean](""+node.asInstanceOf[CheckBox].selectedProperty().get(),false))
		}

		override def set_gui_value(set_value:Data)
		{			
			node.asInstanceOf[CheckBox].selectedProperty().set(
				Utils.parse[Boolean](set_value.asInstanceOf[StringData].value,false))
		}
	}

	class MyScrollPane extends MyComponent
	{
		override def build
		{
			node=new ScrollPane()

			if(properties.contains("hbarp"))
			{
				val hbarp=gs("hbarp","always")
				if(SCROLLBARPOLICIES.contains(hbarp))
					node.asInstanceOf[ScrollPane].setHbarPolicy(SCROLLBARPOLICIES(hbarp))
			}
			if(properties.contains("vbarp"))
			{
				val vbarp=gs("vbarp","always")
				if(SCROLLBARPOLICIES.contains(vbarp))
					node.asInstanceOf[ScrollPane].setVbarPolicy(SCROLLBARPOLICIES(vbarp))
			}
		}
	}

	class MyTabPane extends MyComponent
	{
		override def build
		{
			node=new TabPane()
		}
	}

	class MyTab extends MyComponent
	{
		override def build{}
	}

	class MyWebView extends MyComponent
	{
		override def build
		{
			node=new WebView()

			if(properties.contains("width"))
			{
				node.asInstanceOf[WebView].setMinWidth(gd("width",0.0))
				node.asInstanceOf[WebView].setMaxWidth(gd("width",0.0))
			}

			if(properties.contains("height"))
			{
				node.asInstanceOf[WebView].setMinHeight(gd("height",0.0))
				node.asInstanceOf[WebView].setMaxHeight(gd("height",0.0))
			}

			node.asInstanceOf[WebView].setOnMouseClicked(new EventHandler[MouseEvent]
			{			
				def handle(mouseEvent:MouseEvent)
				{

					var x:Int=mouseEvent.getX().toInt
		            var y:Int=mouseEvent.getY().toInt
		            var etype:String=mouseEvent.getEventType().toString()

		            if(etype=="MOUSE_CLICKED")
		            {
		            	handler(MyEvent(id,"webview clicked","",x,y))
		            }

		        }
		    })
		}
	}

	def build_mycomponent_recursive(parent:MyComponent,handler:THandler)(t:Tag):MyComponent=
	{

		/////////////////////////////////////

		val kind=t.kind

		/////////////////////////////////////

		var c:MyComponent=null

		/////////////////////////////////////

		kind match
		{
			case "vbox" => c=new MyVBox
			case "hbox" => c=new MyHBox
			case "label" => c=new MyLabel
			case "button" => c=new MyButton
			case "combo" => c=new MyCombo
			case "menubar" => c=new MyMenuBar
			case "menu" => c=new MyMenu
			case "menuitem" => c=new MyMenuItem
			case "colorpicker" => c=new MyColorPicker
			case "grid" => c=new MyGridPane
			case "slider" => c=new MySlider
			case "textfield" => c=new MyTextField
			case "textarea" => c=new MyTextArea
			case "checkbox" => c=new MyCheckBox
			case "scrollpane" => c=new MyScrollPane
			case "tabpane" => c=new MyTabPane
			case "tab" => c=new MyTab
			case "webview" => c=new MyWebView
			case _ => println("error "+kind)
		}

		/////////////////////////////////////

		c.tag=t
		c.properties=t.attributes
		c.id=c.gs("id","")
		c.profile=c.gs("profile","")
		c.action=c.gs("action","")
		c.parent=parent
		c.handler=handler
		c.children=t.children.map(build_mycomponent_recursive(parent,handler))

		/////////////////////////////////////

		c.build_all

		if(c.id!="")
		{
			components+=(c.id->c)
		}

		/////////////////////////////////////

		c

		/////////////////////////////////////

	}

	def build_node_recursive(c:MyComponent):Node=
	{
		//println("building "+c.tag.toPrintable())

		val node=c.node

		if(c.isInstanceOf[MyGridPane])
		{
			for(child<-c.children)
			{
				val col=child.gi("c",0)
				val row=child.gi("r",0)

				val gnode=build_node_recursive(child)

				node.asInstanceOf[GridPane].add(gnode,col,row)

				if(child.properties.contains("cs"))
				{
					val colspan=child.gi("cs",1)

					GridPane.setColumnSpan(gnode,colspan)
				}

				if(child.properties.contains("rs"))
				{
					val rowspan=child.gi("rs",1)

					GridPane.setColumnSpan(gnode,rowspan)
				}
			}
		}
		else if(c.isInstanceOf[MyTabPane])
		{
			for(child<-c.children) if(child.children.length>0)
			{
				val tab=new Tab()

				val gnode=build_node_recursive(child.children(0))

				val caption=child.gs("caption","")

				tab.setText(caption)

				tab.setContent(gnode)

				tab.setClosable(false)

				node.asInstanceOf[TabPane].getTabs().add(tab)
			}
		}
		else if(node.isInstanceOf[Pane])
		{
			for(child<-c.children)
			{
				node.asInstanceOf[Pane].getChildren().add(build_node_recursive(child))
			}
		}
		else if(c.isInstanceOf[MyMenuBar])
		{
			for(menu<-c.children)
			{
				for(menu_item<-menu.children)
				{
					menu.menu_node.getItems().add(menu_item.menu_item_node)

					menu_item.menu_item_node.setOnAction(new EventHandler[ActionEvent]
					{
			            def handle(ev:ActionEvent)
						{
							menu_item.handler(new MyEvent(menu_item.id,"menuitem clicked",menu_item.gs("text")))
						}
					})
				}

				c.node.asInstanceOf[MenuBar].getMenus().add(menu.menu_node)
			}
		}
		else if((c.isInstanceOf[MyScrollPane])&&(c.children.length>0))
		{
			val gnode=build_node_recursive(c.children(0))

			node.asInstanceOf[ScrollPane].setContent(gnode)
		}

		node
	}

	def build(path:String,handler:THandler=default_handler):Parent=
	{
		val t=Tag.loadXML(path)

		val c=build_mycomponent_recursive(null,handler)(t)

		val node=build_node_recursive(c)

		node.asInstanceOf[Parent]
	}
}