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

	FromData(enginedata)

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

			if(pathdata.isInstanceOf[StringData])
			{
				path=pathdata.asInstanceOf[StringData].value
			}
		}
	}

	def ToData:Data=
	{
		var mapdata=Map[String,Data]()
		mapdata+=("path" -> StringData(path))
		MapData(mapdata)
	}

	def ReportHTML:String=
	{
		s"""
			|<table>
			|<tr>
			|<td><input type="button" value="..." onclick="idstr='$id'; command='editpath';"></td>
			|<td>path</td><td><font color='blue'>$path</font></td>
			|<td><input type="button" value="X" onclick="idstr='$id'; command='del';"></td>
			|</tr>
			|</table>
		""".stripMargin
	}
}

case class GEngineList(updatecallback:(String)=>Unit)
{
	var enginelist=List[GEngine]()

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

	def Handle(e:WebEngine)
	{
		val command=e.executeScript("command").toString()
		val idstr=e.executeScript("idstr").toString()

		println("command: "+command+" idstr: "+idstr)

		if(command=="add")
		{
			enginelist=enginelist:+GEngine(enginelist.length)
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
	}

	def Update
	{
		updatecallback(ReportHTML)
		Save
	}

	def Load
	{
		enginelist=List[GEngine]()
		val enginelistdata=Builder.getcveval("enginelist").asInstanceOf[SeqData]
		if(enginelistdata != null)
		{
			var i= -1
			enginelist=for(enginedata<-enginelistdata.seq.toList) yield  { i+=1; GEngine(i,enginedata) }
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
			|</script>
			|Under construction!<br>
			|<input type="button" value="Add new engine" onclick="command='add';""><br>
			|$listhtml
		""".stripMargin
	}
}

////////////////////////////////////////////////////////////////////