package data

object Utils
{
	def tabs(level:Int)=Iterator.continually(" ").take(level).mkString

	case class ParseOp[T](op: String => T)

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

    def parsestr(s:String,d:String):String=
    {
    	if(s==null) return d
    	return s
    }
}

case class Tag(kind:String="",children:Seq[Tag]=Seq(),attributes:Map[String,String]=Map(),text:String="")
{
	def toPrintable(level:Int=0,buff:String=""):String=
	{
		val childrenPrintable=((for(child<-children) yield child.toPrintable(level+1,buff)).mkString("\n"))
		buff+Utils.tabs(level)+kind+" "+attributes+" "+text+(if(children.length==0) "" else "\n"+childrenPrintable)
	}

	def getAttribute(key:String,default:String=null):String=
	{
		if(attributes.contains(key)) return attributes(key)
		default
	}
}

object Tag
{
	def parseXML(e:scala.xml.Elem):Tag=
	{
		def getAttributes(e:scala.xml.Elem):Map[String,String]=
		{
			var attr=e.attributes
			val it=Iterator.continually(attr).takeWhile(curr_attr=>
			{
				if(attr.next!=null) attr=attr.next
				curr_attr.next!=null
			})
			(for(attr<-it) yield (attr.key->attr.value.mkString)).toMap
		}

		val attributes=getAttributes(e)

		val children=(for(child<-e.child if(child.isInstanceOf[scala.xml.Elem]))
			yield parseXML(child.asInstanceOf[scala.xml.Elem])).toSeq

		Tag(e.label,children,attributes,if(e.child.length==1) e.text else "")
	}

	def loadXML(path:String):Tag=
	{
		parseXML(scala.xml.XML.loadFile(path))
	}
}

sealed trait Data
{
	def toPrintable(level:Int=0,buff:String=""):String
}

case class StringData(value:String) extends Data
{
	def toPrintable(level:Int=0,buff:String=""):String=
	{
		buff+Utils.tabs(level)+"string\n"+Utils.tabs(level+1)+value
	}
}
case class SeqData(seq:Seq[Data]) extends Data
{
	def toPrintable(level:Int=0,buff:String=""):String=
	{
		val seqPrintable=(for(d<-seq) yield 
			if(d!=null) d.toPrintable(level+1,buff) else Utils.tabs(level)+"!null").mkString("\n")
		buff+Utils.tabs(level)+"seq"+(if(seq.length==0) "" else "\n"+seqPrintable)
	}
}
case class MapData(map:Map[String,Data]) extends Data
{
	def toPrintable(level:Int=0,buff:String=""):String=
	{
		val mapPrintable=(for((k,v)<-map) yield Utils.tabs(level+1)+k+"->\n"+v.toPrintable(level+2,buff)).mkString("\n")
		buff+Utils.tabs(level)+"map"+(if(map.keys.toList.length==0) "" else "\n"+mapPrintable)
	}
}

object Data
{
	def parseTag(t:Tag):Data=
	{
		t.kind match
		{
			case "v" => StringData(t.text)
			case "l" => SeqData(t.children.map(parseTag).toSeq)
			case "m" => MapData(t.children.map(child=>(child.getAttribute("key")->parseTag(child))).toMap)
			case _ => StringData("")
		}
	}

	def fromList(list:List[String]):SeqData=
	{
		SeqData(list.map(e=>StringData(e)).toSeq)
	}

	def toList(seqdata:SeqData):List[String]=
	{
		seqdata.seq.map(e=>e.asInstanceOf[StringData].value).toList
	}

	def toXML(what:Data,key:String=null):scala.xml.Elem=
	{
		if(what==null) return <v>NULL</v>

		if(what.isInstanceOf[StringData])
		{
			val innerXML=what.asInstanceOf[StringData].value
			return if(key==null) <v>{innerXML}</v>
			else <v key={key}>{innerXML}</v>
		}

		if(what.isInstanceOf[SeqData])
		{
			val innerXML=for(d<-what.asInstanceOf[SeqData].seq) yield toXML(d)
			return if(key==null) <l>{innerXML}</l>
			else <l key={key}>{innerXML}</l>
		}

		if(what.isInstanceOf[MapData])
		{
			val innerXML=for((k,v)<-what.asInstanceOf[MapData].map) yield toXML(v,k)
			return if(key==null) <m>{innerXML}</m>
			else <m key={key}>{innerXML}</m>
		}

		// we should not get here
		<v>INVALID</v>
	}

	def toXMLPretty(what:Data):String=
	{
		new scala.xml.PrettyPrinter(80,3).format(toXML(what))
	}

	def saveXML(what:Data,path:String)
	{
		scala.xml.XML.save(path,toXML(what))
	}

	def saveXMLPretty(what:Data,path:String)
	{
		org.apache.commons.io.FileUtils.writeStringToFile(
				new java.io.File(path),
				toXMLPretty(what),
				null.asInstanceOf[String]
			)
	}

	def set(orig:Data,pathList:List[String],value:Data):Data=
	{	

		if(pathList.length<=0)
		{
			return value
		}

		val head=pathList.head
		val tail=pathList.tail

		val index=Utils.parse[Int](head,-1)

		if(index>=0)
		{
			// list
			val orig_seq:SeqData=
				if((orig!=null)&&(orig.isInstanceOf[SeqData])) orig.asInstanceOf[SeqData]
				else SeqData(Seq[Data]())

			val required_length=index+1
			val actual_length=orig_seq.seq.length
			val needed_extra_length=required_length-actual_length

			val extra_seq=Iterator.continually(null.asInstanceOf[Data]).take(needed_extra_length).toSeq

			val extended_seq=orig_seq.seq++extra_seq

			val extended_seq_updated=(for(i<-0 to extended_seq.length-1) yield 
				if(i!=index) extended_seq(i) else set(extended_seq(index),tail,value)).toSeq

			return SeqData(extended_seq_updated)
		}
		else
		{
			// map
			val orig_map:MapData=
				if((orig!=null)&&(orig.isInstanceOf[MapData])) orig.asInstanceOf[MapData]
				else MapData(Map[String,Data]())

			val orig_val=if(orig_map.map.contains(head)) orig_map.map(head) else null.asInstanceOf[Data]

			val updated_map=orig_map.map+(head->set(orig_val,tail,value))

			return MapData(updated_map)
		}
	}

	def set(orig:Data,path:String,value:Data):Data=
	{
		val pathList=path.split("#").toList

		set(orig,pathList,value)
	}

	def get(orig:Data,pathList:List[String]):Data=
	{
		if(orig==null) return null

		if(pathList.length<=0) return orig

		val head=pathList.head
		val tail=pathList.tail

		val index=Utils.parse[Int](head,-1)

		if(index>=0)
		{
			// list
			if(!orig.isInstanceOf[SeqData]) return null

			val required_length=index+1
			val orig_seq=orig.asInstanceOf[SeqData].seq
			val actual_length=orig_seq.length

			if(actual_length< required_length) return null

			return get(orig_seq(index),tail)
		}
		else
		{
			// map
			if(!orig.isInstanceOf[MapData]) return null

			val orig_map=orig.asInstanceOf[MapData].map

			if(!orig_map.contains(head)) return null

			return get(orig_map(head),tail)
		}
	}

	def get(orig:Data,path:String):Data=
	{
		val pathList=path.split("#").toList

		get(orig,pathList)
	}

	def parseXML(e:scala.xml.Elem):Data=
	{
		parseTag(Tag.parseXML(e))
	}

	def loadXML(path:String):Data=
	{
		if(new java.io.File(path).exists) parseXML(scala.xml.XML.loadFile(path))
		else MapData(Map[String,Data]())
	}

	def parsePatch(orig:Data,patches:Tag):Data=
	{
		var current=orig

		for(patch<-patches.children)
		{
			val path=patch.getAttribute("p")
			if((path!=null)&&(patch.children.length>0))
			{
				var value=parseTag(patch.children(0))
				if(patch.kind=="m")
				{
					val old_value=get(current,path)
					if(old_value!=null) value=old_value
				}
				current=set(current,path,value)
			}
		}

		current
	}

	def parsePatchXML(orig:Data,e:scala.xml.Elem):Data=
	{
		parsePatch(orig,Tag.parseXML(e))
	}

	def loadPatchXML(orig:Data,path:String):Data=
	{
		parsePatchXML(orig,scala.xml.XML.loadFile(path))
	}

	def gs(orig:Data,path:String,default:String=null):String=
	{
		val v=get(orig,path)
		if((v!=null)&&(v.isInstanceOf[StringData])) return v.asInstanceOf[StringData].value
		default
	}

	def gi(orig:Data,path:String,default:Int=0):Int=
	{
		val v=get(orig,path)
		if((v!=null)&&(v.isInstanceOf[StringData])) return Utils.parse[Int](v.asInstanceOf[StringData].value,default)
		default
	}

	def gd(orig:Data,path:String,default:Double=0):Double=
	{
		val v=get(orig,path)
		if((v!=null)&&(v.isInstanceOf[StringData])) return Utils.parse[Double](v.asInstanceOf[StringData].value,default)
		default
	}
}