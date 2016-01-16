package utils

import java.io._
import java.lang.System

import scala.io.Source

import scala.xml._

import java.awt.datatransfer._
import java.awt.Toolkit
import java.io._

import scala.collection.mutable.ArrayBuffer

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

class Timer
{
	var t0=System.nanoTime()
	def elapsed:Double = (System.nanoTime() - t0)/1.0e9
}

object HeapSize
{
	def heapsize = "heap size "+Runtime.getRuntime().totalMemory()/1000000
}

object ClipboardSimple extends java.awt.datatransfer.ClipboardOwner
{

	override def lostOwnership(aClipboard:Clipboard,aContents:Transferable)
	{
		//do nothing
	}

	def clipget:String=getClipboardContents

	def getClipboardContents:String=
	{
		var result=""

		val clipboard = Toolkit.getDefaultToolkit().getSystemClipboard()

		val contents = clipboard.getContents(null)

		val hasTransferableText =
			(contents != null) &&
			contents.isDataFlavorSupported(DataFlavor.stringFlavor)

		if(hasTransferableText)
		{
			try
			{
				result = contents.getTransferData(DataFlavor.stringFlavor).toString
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
		val stringSelection = new StringSelection(content)

		val clipboard = Toolkit.getDefaultToolkit().getSystemClipboard()

		clipboard.setContents(stringSelection, this)
	}

}

object Parse
{

	case class ParseOp[T](op: String => T)

	implicit val popString = ParseOp[String](_.mkString)
	implicit val popInt = ParseOp[Int](_.toInt)
	implicit val popDouble = ParseOp[Double](_.toDouble)
	implicit val popBoolean = ParseOp[Boolean](_.toBoolean)
	
	def parse[T: ParseOp](s: String,d: T)=
	{
		try
		{
			implicitly[ParseOp[T]].op(s)
		}
    	catch
    	{
    		case _ : Throwable => d
    	}
    }

    def parseXml[T: ParseOp](x: NodeSeq,d: T) = parse[T](x.text,d)

    def parseXmlStr(x: NodeSeq,d: String):String=
    {
    	val t=x.text

    	if(t!=null)
    	{
    		if(t!="") return t
    	}
    	else
    	{
    		return d
    	}

    	d
    }

	//////////////////////////////////////////////

	def strip(content: String):String=
		content.replaceAll("[\r\n]","")

	def isValue(what:String):Boolean =
		(what!="NA")&&(what!="")

	def myToFloat(what: String):Float =
	{
		if((what=="NA")||(what=="")||(what=="0"))
		{
			return 0.toFloat
		}
		val whats=what.replaceAll("^0+","")
		val floatmatch=""",[0-9]{2}$""".r.unanchored
		whats match
			{
				case floatmatch(_*) => return whats.split(",").mkString.toFloat/100
				case _ =>
			}
		whats.toFloat
	}

	def isInt(what: String):Boolean =
	{
		try
		{
			val i=what.toInt
		}
		catch
		{
			case ex:NumberFormatException=>return false
		}
		return true
	}

	def myToDouble(what:String):Double =
	{
		if(isInt(what))
		{
			return what.toInt.toDouble
		}
		if(isDouble(what))
		{
			return what.toDouble
		}
		return myToFloat(what).toDouble
	}

	def isDouble(what: String):Boolean =
	{
		if(isInt(what))
		{
			return true
		}

		try
		{
			val i=what.toDouble
		}
		catch
		{
			case ex:NumberFormatException=>return false
		}
		return true
	}
}

object Dir
{

	def mkdirs(path: List[String]) = // return true if path was created
		path.tail.foldLeft(new File(path.head)){(a,b) => a.mkdir; new File(a,b)}.mkdir
		
	def mkdir(path: String) = // create single dir
		mkdirs(List(path))

	def getListOfFiles(dir: String):List[File] =
	{
		val d = new File(dir)
		if (d.exists && d.isDirectory)
		{
			d.listFiles.filter(_.isFile).toList
		}
		else
		{
			List[File]()
		}
	}
	
	def getListOfFileNames(dir: String):List[String] =
		for(f<-getListOfFiles(dir)) yield f.getName

	def getListOfFileNamesWithExt(dir:String,ext:String):List[String]=
	{
		var l=ArrayBuffer[String]()
		for(name<-getListOfFileNames(dir))
		{
			val parts=name.split("\\.")
			if(parts.length==2)
			{
				if(parts(1)==ext)
				{
					l+=parts(0)
				}
			}
		}
		l.toList
	}
		
	def deleteFilesInDir(dir: String)
	{
		for(f<-getListOfFiles(dir)) f.delete
	}

	def saveTxt(name: String,content: String)
	{
		val writer=new PrintWriter(new File(name))
		writer.write(content)
		writer.close()
	}

	def readTxtLinesVerbose(path:String):Array[String]=
	{
		val timer=new Timer
		println("reading lines of %s , %s".format(path,HeapSize.heapsize))
		val lines=Source.fromFile(path).getLines().toArray
		println("number of lines %d , elapsed %f , %s".format(lines.length,timer.elapsed,HeapSize.heapsize))
		lines
	}

	def readTxtLines(path:String):Array[String]=
	{
		Source.fromFile(path).getLines().toArray
	}

	def readTxt(path:String):String =
	{
		Source.fromFile(path).getLines().mkString("\n")
	}

	def htmlify(path: String)
	{
		val hpath=path.replaceAll("\\.txt$",".html")
		
		val lines=readTxtLines(path)
		
		var html="<table border=1>"
		
		html=html+(for(line<-lines) yield "<tr><td>"+line.split("\t").mkString("</td><td>")+"</td></tr>\n").mkString
		
		html=html+"</table>"
		
		saveTxt(hpath,html)
	}

	def parseTxtSmart(path: String):Array[Map[String,String]]=
	{
		val lines=Source.fromFile(path).getLines().toArray
		
		val headers=Parse.strip(lines.head).split("\t");
		
		for(line<-lines.tail) yield
			(headers zip Parse.strip(line).split("\t")).toMap
	}

	def parseRecord(path: String):Map[String,String]=
	{
		val kvs=parseTxtSmart(path)

		var m=Map[String,String]()

		for(kv<-kvs)
		{
			m+=(kv("key")->kv("value"))
		}

		m
	}

}

object Log
{
	var do_log=true
	def log(what: String,force: Boolean=false)
	{
		if((do_log)||force)
		{
			println(what)
		}
	}
}