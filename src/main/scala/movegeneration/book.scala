package book

import utils.Dir._
import utils.Parse._
import scala.xml._

import gui2.Encode32._

import java.io._
import java.nio.file._

import settings._

import scala.io.Source

import org.apache.commons.io.FileUtils._

object butils
{

	var interrupted=false

	def list_books(v:String=settings.getvariant):List[String]=
	{
		getListOfFileNamesWithExt(s"books/$v","txt")
	}

	def default_log_callback(what:String)
	{
		println(what)
	}

	var log_callback:(String)=>Unit=default_log_callback

	def del_book(db:String,v:String=settings.getvariant)
	{

		interrupted=false

		val b=new book(List("book",v))

		val pl=new PosList(db)

		for(fen<-pl.pl.keys)
		{
			if(interrupted) return

			log_callback("deleting position "+fen)

			b.loadPos(fen,loadcurrent=false)

			if(b.booklist.remove_book(db))
			{
				log_callback("done")
			}

			b.savePos(addcurrent=false)
		}

		val path=(pl.path.split("/")).mkString(File.separator)
		
		val f=new File(path)

		log_callback("deleting "+path)
		
		try
		{
			f.setWritable(true)
			f.delete()
		}
		catch
		{
			case e:NoSuchFileException => log_callback("no such file")
			case e:IOException => log_callback("io exception")
			case e:Throwable => log_callback("exception")
		}

	}
}

case class bookEntry(
	var san:String="",
	var plays:Int=0,
	var annot:String="",
	var wins:Int=0,
	var draws:Int=0,
	var losses:Int=0
	)
{
	def toXml=
	{
		<move san={san}>
		<plays>{plays}</plays>
		<annot>{annot}</annot>
		<wins>{wins}</wins>
		<draws>{draws}</draws>
		<losses>{losses}</losses>
		</move>
	}

	def fromXml(move:NodeSeq)
	{
		san=(move \ "@san").text

		plays=parseXml[Int](move \ "plays",0)
		annot=parseXml[String](move \ "annot","")
		wins=parseXml[Int](move \ "wins",0)
		draws=parseXml[Int](move \ "draws",0)
		losses=parseXml[Int](move \ "losses",0)
	}
}

case class bookPosition(
	var fen:String
	)
{

	var entries=Map[String,bookEntry]()

	var games=""

	def update_result(san:String,result:String)
	{
		val win=(result=="1-0")
		val draw=(result=="1/2-1/2")
		val loss=(result=="0-1")

		if(entries.contains(san))
		{
			if(win) entries(san).wins+=1
			if(draw) entries(san).draws+=1
			if(loss) entries(san).losses+=1
		}
		else
		{
			if(win) entries+=(san->bookEntry(san=san,wins=1))
			if(draw) entries+=(san->bookEntry(san=san,draws=1))
			if(loss) entries+=(san->bookEntry(san=san,losses=1))
		}
	}

	def inc_move_count(san:String)
	{
		if(entries.contains(san))
		{
			entries(san).plays+=1
		}
		else
		{
			entries+=(san->bookEntry(san=san,plays=1))
		}
	}

	def annot(san:String,annot:String)
	{
		if(entries.contains(san))
		{
			entries(san).annot=annot
		}
		else
		{
			entries+=(san->bookEntry(san=san,annot=annot))
		}
	}

	def del(san:String)
	{
		if(entries.contains(san))
		{
			entries-=san
		}
	}

	def game_found(gameMd5:String):Boolean=((gameMd5!="")&&games.contains(gameMd5))

	def add_game(gameMd5:String)
	{

		if(gameMd5=="") return

		if(game_found(gameMd5)) return

		if(games=="")
		{
			games=gameMd5
		}
		else
		{
			games+=("_"+gameMd5)
		}
		
	}

	def toXml=
	{
		val entry_list=(for((k,v)<-entries) yield v.toXml).toList

		{
			<position fen={fen}>
			<movelist>
				{entry_list}
			</movelist>
			<games>{games}</games>
			</position>
		}
	}

	def fromXml(xml:NodeSeq)
	{

		entries=Map[String,bookEntry]()

		fen=(xml \ "position" \ "@fen").text

		//println("loading position fen "+fen)

		val moves=xml \ "position" \ "movelist" \ "move"
		
		for(move<-moves)
		{

			val entry=bookEntry()

			entry.fromXml(move)

			entries+=(entry.san->entry)

		}

		games=parseXml[String](xml \ "position" \ "games","")

	}

	def toPrintable(html:Boolean=false):String=
	{

		def sortfunc(ak:String,bk:String):Boolean=
		{
			val a=entries(ak)
			val b=entries(bk)
			val aa=settings.ANNOTATIONS.reverse.indexOf(a.annot)
			val ba=settings.ANNOTATIONS.reverse.indexOf(b.annot)
			if(aa!=ba) return aa > ba
			entries(ak).plays>entries(bk).plays
		}

		val keys=entries.keys.toArray.sortWith(sortfunc)

		if(!html) return(
		"\n%-10s | %5s | %5s | %5s | %5s".format("move","plays","1-0","draw","0-1")+
		"\n-----------+-------+-------+-------+-------\n"+
		(for(k<-keys) yield {
			"%-10s | %5d | %5d | %5d | %5d".format(k+" "+entries(k).annot,entries(k).plays,entries(k).wins,entries(k).draws,entries(k).losses)
			}).mkString("\n")+"\n"
		)

		val td="""<td align="center">"""

		val items=
		(for(k<-keys) yield {

			val annot=entries(k).annot
			val plays=entries(k).plays
			val wins=entries(k).wins
			val draws=entries(k).draws
			val losses=entries(k).losses

			val col=settings.get_annot_color(annot)

			val annots=(for(a<-settings.ANNOTATIONS) yield
			{
				val acol=settings.get_annot_color(a)
				s"""
					|<td align="center" onmousedown="setclick('$k','annot','$a');">
					|<font color="$acol" size="5">$a</font>
					|</td>
				""".stripMargin
			}).mkString("\n")

			s"""
				|<tr>
				|<td align="center" onmousedown="setclick('$k','make','');">
				|<a name="dummy"></a>
				|<a href="#dummy" style="text-decoration: none;">
				|<font color="$col" size="6"><b>
				|$k
				|</b></font>
				|</a>
				|</td>
				|$td<font color="$col" size="5"><b>$annot</b></font></td>
				|$td<font color="#000000" size="5"><b>$plays</b></font></td>
				|$td<font color="#007f00" size="5"><b>$wins<b></font></td>
				|$td<font color="#00007f" size="5"><b>$draws<b></font></td>
				|$td<font color="#7f0000" size="5"><b>$losses<b></font></td>
				|$annots
				|$td<font color="#ff0000" size="5" onmousedown="setclick('$k','del','');">X</font></td>
				|</tr>
			""".stripMargin
			}).mkString("\n")

		val numannots=settings.ANNOTATIONS.length

		s"""
			|<script>
			|var key="";
			|var action="";
			|var param="";
			|function setclick(setkey,setaction,setparam)
			|{
			|	key=setkey;
			|	action=setaction;
			|	param=setparam;
			|}
			|</script>
			|<table border="0" cellpadding="3" cellspacing="3">
			|<tr>
			|$td <i>move</i></td>
			|$td <i>annot</i></td>
			|$td <i>plays</i></td>
			|$td <i>white wins</i></td>
			|$td <i>draws</i></td>
			|$td <i>black wins</i></td>
			|<td align="center" colspan="$numannots"><i>annotate</i></td>
			|<td align="center"><i>del</i></td>
			|</tr>
			|$items
			|</table>
		""".stripMargin
	}

}

case class bookList(var fen:String)
{

	def name=encode(fen,true)

	var books=Map[String,bookPosition]()

	def remove_book(name:String):Boolean=
	{
		if(books.contains(name))
		{
			books=books-name
			return true
		}

		false
	}

	def toXml=
	{
		<booklist fen={fen}>
			{
				for((name,book)<-books) yield 
				{
					<book name={name} fen={fen}>{book.toXml}</book>
				}
			}
		</booklist>
	}

	def fromXml(xml:NodeSeq)
	{
		books=Map[String,bookPosition]()

		fen=(xml \ "@fen").text

		//println("loading booklist for fen "+fen)

		val booksXml=xml \ "book"

		for(bookXml<-booksXml)
		{
			val bp=bookPosition("")

			val name=(bookXml \ "@name").text

			//println("loading book "+name+" = "+bookXml)

			bp.fromXml(bookXml)

			books+=(name->bp)
		}
	}
}

case class book(
	pathlist:List[String]=List[String]()
	)
{

	if(pathlist.length>0)
	{
		mkdirs(pathlist)
	}

	val path=pathlist.mkString("/")

	var currentPos=bookPosition("")

	var booklist=bookList("")

	var current_book="default"

	def loadPos(fen:String,loadcurrent:Boolean=true)
	{

		booklist=bookList(fen)

		current_book=settings.get_current_book()

		var p=bookPosition(fen)

		val fullpath=path+"/"+booklist.name+".xml"

		if(new File(fullpath).exists)
		{

			//println("loading pos from "+fullpath)

			val xml=scala.xml.XML.loadFile(fullpath)

			booklist.fromXml(xml)

			if(loadcurrent)
			{

				if(booklist.books.contains(current_book))
				{
					//println("book found")

					p=booklist.books(current_book)
				}

			}

		}

		currentPos=p

	}

	def savePos(addcurrent:Boolean=true)
	{

		val fullpath=path+"/"+booklist.name+".xml"

		if(addcurrent) booklist.books+=(current_book->currentPos)

		val xml=booklist.toXml

		scala.xml.XML.save(fullpath,xml)

		if(addcurrent)
		{

			val pl=PosList(current_book)

			pl.add(currentPos.fen)

			pl.save

		}

	}

}

case class PosList(cb:String,v:String=settings.getvariant)
{

	val path=s"books/$v/$cb.txt"

	var pl=Map[String,Boolean]()

	load

	def add(fen:String)
	{
		pl+=(fen->true)
	}

	def load
	{
		pl=Map[String,Boolean]()

		if(new File(path).exists)
		{
			val content=readFileToString(new File(path),null.asInstanceOf[String])
			for(line<-content.split("\n"))
			{
				pl+=(line->true)
			}
		}
		else
		{
			save
		}
	}

	def save
	{
		mkdirs(List("books",v))

		val content=(for((k,v)<-pl) yield k).mkString("\n")

		saveTxt(path,content)
	}

}