package game

import board._
import move._
import square._
import piece.piece._
import book._
import utils.Timer
import utils.Dir._
import utils.Parse._
import scala.xml._

import java.io._

import scala.collection.mutable.ArrayBuffer

import org.apache.commons.codec.digest.DigestUtils._
import org.apache.commons.io.FileUtils._

import settings.settings._
import builder._

case class GameResult(
	var resultint:Int=0,
	var resultstr:String="1/2-1/2",
	var resultreason:String="draw by threefold repetition"
)
{

}

case class gameNode(
	genSan:String="",
	fen:String="",
	genPriority:Int=0,
	parent:gameNode=null,
	fullmove_number:Int=0,
	turn:Char=' ',
	num_checks:Map[TColor,Int]=Map(WHITE->0,BLACK->0),
	genAlgeb:String="",
	genTrueAlgeb:String="",
	var comment:String=""
	)
{

	var priority=0
	var childs=Map[String,gameNode]()

	def sortedSans:List[String]=childs.keys.toList.sortWith(childs(_).genPriority < childs(_).genPriority)

	def commented_san:String=
	{
		if(comment=="") return genSan
		s"$genSan {$comment}"
	}

	def get_move_no:String=
	{
		if(parent==null)
		{
			return ""
		}
		var move_no=""
		if(parent.fullmove_number>0)
		{
			move_no=parent.fullmove_number+"."
			if(parent.turn=='b') move_no+=" ..."
		}
		move_no
	}

	def get_fullmove_number:Int=
	{
		if(parent==null)
		{
			return 0
		}
		return parent.fullmove_number
	}

	def get_turn:Char=
	{
		if(parent==null)
		{
			return ' '
		}
		return parent.turn
	}
}

object game
{
}

class game
{

	import game._

	var b=new board

	var root=gameNode()

	var current_node=gameNode()

	var pgn_headers=Map[String,String]()

	var book=new book()

	var current_gameMd5=""

	var PARSING=false
	var BUILDING=false

	/////////////////////////////////////////////////////////////

	reset

	/////////////////////////////////////////////////////////////

	def report_trunc_fen:String=b.report_trunc_fen

	def truncate_fen(fen:String):String=
	{
		val parts=fen.split(" ").toList
		val trunc_fen=parts(0)+" "+parts(1)+" "+parts(2)+" "+parts(3)
		trunc_fen
	}

	def HasMoves:Boolean= ( root.childs.keys.toList.length > 0 )

	def GetOpening:String=
	{
		if(!HasMoves) return "*"
		val dummy=new game
		dummy.set_from_fen(root.fen)
		var ptr=root
		var cnt=0
		while((ptr.childs.keys.toList.length>0)&&(cnt< 3))
		{
			ptr=ptr.childs(ptr.sortedSans(0))
			dummy.makeSanMove(ptr.genSan)
			cnt+=1
		}
		dummy.current_line_pgn
	}

	def is_from_startpos:Boolean=
	{
		if(getvariant=="Chess960") return false
		val dummy=new board
		dummy.reset
		val start_fen=dummy.report_fen
		val root_fen=root.fen
		start_fen==root_fen
	}

	def report_result:GameResult=
	{
		val bclone=b.cclone
		bclone.initMoveGen
		val haslegal=bclone.nextLegalMove
		if(haslegal)
		{
			var fen_cnt=1
			var node_ptr=current_node
			val trunc_fen=truncate_fen(node_ptr.fen)
			while((node_ptr.parent!=null)&&(fen_cnt< 3))
			{
				node_ptr=node_ptr.parent
				if(truncate_fen(node_ptr.fen)==trunc_fen) fen_cnt+=1
			}
			if(fen_cnt>=3)
			{
				return GameResult(0,"1/2-1/2","GUI adjudication: draw by threefold repetition")
			}
			if(bclone.halfmove_clock>=100)
			{
				return GameResult(0,"1/2-1/2","GUI adjudication: draw by the fifty move rule")
			}
			return null
		} else if(bclone.isInCheck) {
			if(bclone.turn==WHITE)
			{
				if(bclone.whereIsKing(WHITE)!=square.NO_SQUARE)
				{
					return GameResult(-1,"0-1","GUI adjudication: white checkmated")
				} else {
					return GameResult(-1,"0-1","GUI adjudication: white king destroyed")
				}
			} else {
				if(bclone.whereIsKing(BLACK)!=square.NO_SQUARE)
				{
					return GameResult(1,"1-0","GUI adjudication: black checkmated")
				} else {
					return GameResult(1,"1-0","GUI adjudication: black king destroyed")
				}
			}
		} else {
			return GameResult(0,"1/2-1/2","GUI adjudication: stalemate")
		}
	}

	def from_pgn_and_current_line(pgn:String,currentline:String)
	{

		if(pgn!="")
		{
			set_from_pgn(pgn)
		}

		tobegin

		if(currentline!="")
		{				
			for(san<-currentline.split(" "))
			{
				makeSanMove(san)
			}
		}

	}

	def current_line_pgn:String=
	{

		var fullmove_number=root.fullmove_number
		var turn=root.turn

		var a=ArrayBuffer[String]()

		var first=true
		for(san<-current_line_moves)
		{
			var psan=san
			if(turn=='w')
			{
				psan=fullmove_number+". "+san
			}
			else if(first==true)
			{
				psan=fullmove_number+". ... "+san
			}
			first=false
			if(turn=='b') fullmove_number+=1
			turn=(if(turn=='w') 'b' else 'w')
			a+=psan
		}

		a.mkString(" ")

	}

	def current_line:String=
	{
		current_line_moves.mkString(" ")
	}

	def current_line_moves:ArrayBuffer[String]=
	{
		var a=ArrayBuffer[String]()

		var cn=current_node

		while(cn!=root)
		{

			a+=cn.genSan

			cn=cn.parent

		}

		a.reverse
	}

	def diff_true_algeb_moves(root_fen:String):ArrayBuffer[String]=
	{
		var a=ArrayBuffer[String]()

		var cn=current_node

		var found=false

		while((cn!=null)&&(!found))
		{

			found=(cn.fen==root_fen)

			if(!found) a+=cn.genTrueAlgeb

			cn=cn.parent

		}

		if(!found) return null

		a.reverse
	}

	def current_line_true_algeb_moves:ArrayBuffer[String]=
	{
		var a=ArrayBuffer[String]()

		var cn=current_node

		while(cn!=root)
		{

			a+=cn.genTrueAlgeb

			cn=cn.parent

		}

		a.reverse
	}

	def book_enabled=Builder.gb("components#bookenabled",true)

	def pos_changed
	{
		if(book_enabled)
		{
			book.loadPos(current_node.fen)
		}
		else
		{
			book.currentPos=bookPosition().FromFen(current_node.fen)
		}
	}

	def set_from_fen_extended(fen:String,set_num_checks:Map[TColor,Int])
	{
		b.set_from_fen(fen)
		b.num_checks=Map(WHITE->set_num_checks(WHITE),BLACK->set_num_checks(BLACK))
	}

	def tonode(gn:gameNode)
	{
		current_node=gn
		set_from_fen_extended(current_node.fen,current_node.num_checks)
		pos_changed
	}

	def tobegin
	{
		current_node=root
		set_from_fen_extended(current_node.fen,current_node.num_checks)
		pos_changed
	}

	def forward_node:Boolean=
	{
		if(current_node.childs.keys.toList.length>0)
		{
			val main_san=current_node.sortedSans(0)
			current_node=current_node.childs(main_san)
			return true
		}
		return false
	}

	def forward
	{
		if(forward_node)
		{
			set_from_fen_extended(current_node.fen,current_node.num_checks)
			pos_changed
		}
	}

	def toend
	{
		while(forward_node)
		{

		}
		set_from_fen_extended(current_node.fen,current_node.num_checks)
		pos_changed
	}

	def back
	{
		if(current_node.parent!=null)
		{
			current_node=current_node.parent
			set_from_fen_extended(current_node.fen,current_node.num_checks)
			pos_changed
		}
	}

	def set_from_fen(fen:String,clear_headers:Boolean=true)
	{

		b.set_from_fen(fen)

		root=gameNode(
				genSan="*",
				fen=fen,
				genPriority=0,
				parent=null,
				fullmove_number=b.fullmove_number,
				turn=colorLetterOf(b.turn)
			)

		current_node=root

		if(clear_headers)
		{
			pgn_headers=Map[String,String]()
		}

		book=new book(List("book",variant))

		if(!BUILDING)
		{
			current_gameMd5=""
		}

		pos_changed

	}

	def reset
	{
		b.reset
		set_from_fen(b.report_fen)
	}

	def delete
	{
		if(current_node.parent!=null)
		{
			val delSan=current_node.genSan
			current_node=current_node.parent
			current_node.childs=current_node.childs-delSan
			b.set_from_fen(current_node.fen)
			pos_changed
		}
	}

	def makeSanMove(san:String,addcomment:String="")
	{
		val m=b.sanToMove(san)
		if(m!=null)
		{
			makeMove(m,addcomment)
		}
	}

	def makeMove(m:move,addcomment:String="")
	{

		if(BUILDING)
		{
			if((build_cutoff>0)&&(b.fullmove_number>build_cutoff)) return
		}

		val san=b.toSan(m)
		val algeb=m.toAlgeb
		val true_algeb=b.to_true_algeb(algeb)

		if(san!=null)
		{
			b.makeMove(m)
			if(current_node.childs.contains(san))
			{
				current_node=current_node.childs(san)
			}
			else
			{
				current_node.priority+=1
				val newNode=gameNode(
						genSan=san,
						fen=b.report_fen,
						genPriority=current_node.priority,
						parent=current_node,
						fullmove_number=b.fullmove_number,
						turn=colorLetterOf(b.turn),
						num_checks=Map(WHITE->b.num_checks(WHITE),BLACK->b.num_checks(BLACK)),
						genAlgeb=algeb,
						genTrueAlgeb=true_algeb,
						comment=addcomment
					)
				current_node.childs+=(san->newNode)
				current_node=newNode
			}

			val game_found=book.currentPos.game_found(current_gameMd5)

			if(inc_move_count)
			{
				//if(!(BUILDING&&game_found))
				{
					book.currentPos.inc_move_count(san)
				}
				
			}

			if(update_result)
			{
				if(!(BUILDING&&game_found))
				{
					if(pgn_headers.contains("Result"))
					{
						book.currentPos.update_result(san,pgn_headers("Result"))
					}
				}
			}

			if(add_games)
			{
				book.currentPos.add_game(current_gameMd5)
			}

			if(book_enabled)
			{
				book.savePos()
			}

			pos_changed

		}

	}

	def report_fen:String=current_node.fen

	def ntabs(n:Int):String=(for(i<-1 to n) yield "\t").toList.mkString

	def report_tree:String=
	{

		var tree=""

		def report_tree_recursive(gn:gameNode,level:Int)
		{

			var move_no=gn.get_move_no
			var current=""
			if(gn==current_node) current="(*)"
			tree+=ntabs(level)+move_no+gn.genSan+current+"\n"

			val sortedSans=gn.sortedSans

			for((san)<-sortedSans)
			{
				report_tree_recursive(gn.childs(san),level+1)
			}
		}

		report_tree_recursive(root,0)

		tree
	}

	def GetTermination:String=
	{
		var term=""

		if(pgn_headers.contains("Result"))
		{
			term+=" "+pgn_headers("Result")
		} else {
			term+=" *"
		}

		if(pgn_headers.contains("Termination"))
		{
			term+=" {"+pgn_headers("Termination")+"}"
		}

		term
	}

	def report_pgn_move_list:String=
	{

		var pgn=""

		def report_pgn_recursive(gn:gameNode,sub:Boolean)
		{

			val sortedSans=gn.sortedSans

			val numSans=sortedSans.length

			var i=0
			for(san<-sortedSans)
			{

				val is_sub=(i>0)

				val child=gn.childs(san)

				var move_no=child.get_move_no

				if(is_sub)
				{
					pgn+="("+move_no
				}
				else if(child.get_turn=='w')
				{
					pgn+=move_no
				}

				//val addsan=child.genSan

				val addsan=child.commented_san

				pgn+=" "+addsan+" "

				//if(child==current_node) pgn+=" {*} "

				if(is_sub) report_pgn_recursive(gn.childs(san),true)

				i+=1

			}

			if(numSans>0)
			{
				val main_child=gn.childs(sortedSans(0))

				val sortedSansMain=main_child.sortedSans

				val numSansMain=sortedSansMain.length

				if((numSansMain>0)&&(numSans>1))
				{
					val mains_main_child=main_child.childs(sortedSansMain(0))

					if(main_child.turn=='b')
					{
						pgn+=" "+mains_main_child.get_move_no+" "
					}
				}

				report_pgn_recursive(main_child,sub)
			}
			else if(sub)
			{
				pgn+=") "
			}

		}

		val dummy=new board
		dummy.set_from_fen(root.fen)

		if(dummy.getturn==BLACK)
		{
			pgn+=dummy.fullmove_number+". ... "
		}
		else if(!HasMoves)
		{
			pgn+=dummy.fullmove_number+". "
		}

		if(HasMoves)
		{
			report_pgn_recursive(root,false)
		}		

		pgn=pgn.replaceAll(" +"," ")
		pgn=pgn.replaceAll(" +\\)",")")

		pgn
	}

	var report_headers=""

	var replen=0

	def report_pgn:String=
	{
		pgn_headers+=("FEN"->root.fen)
		pgn_headers+=("Variant"->getvariant)

		report_headers=(for(k<-SortedPgnHeaderKeys) yield { val v=pgn_headers(k); s"""[$k "$v"]""" } ).mkString("\n")

		replen=report_headers.length+2

		val move_list=report_pgn_move_list

		List(report_headers,move_list).mkString("\n\n")+GetTermination
	}

	def report_pgn_tree:String=
	{

		var pgn=""

		def report_pgn_recursive(gn:gameNode,sub:Boolean,line: List[String])
		{

			val sortedSans=gn.sortedSans

			val numSans=sortedSans.length

			if(numSans==0) {
				pgn+=line.mkString(" ")+"\n"
				return
			}

			for(san<-sortedSans)
			{

				val child=gn.childs(san)

				var move_no=child.get_move_no

				var addsan=san

				if(child.get_turn=='w')
				{
					addsan="%4s".format(move_no)+" %-5s".format(addsan)
				} else {
					addsan=" %-5s".format(addsan)
				}

				report_pgn_recursive(gn.childs(san),true,line:+addsan)

			}

		}

		val dummy=new board
		dummy.set_from_fen(root.fen)

		var line=List[String]()

		if(dummy.getturn==BLACK)
		{
			val fmn=dummy.fullmove_number
			line=line:+s"$fmn. ... "
		}

		report_pgn_recursive(root,false,line)

		pgn
	}

	var html_pgn_nodes=ArrayBuffer[gameNode]()

	def report_pgn_move_list_html(cn:gameNode):String=
	{

		var pgn=""

		html_pgn_nodes=ArrayBuffer[gameNode]()

		def report_pgn_recursive(gn:gameNode,sub:Boolean,level:Int=0)
		{

			val sortedSans=gn.sortedSans

			val numSans=sortedSans.length

			var i=0
			for(san<-sortedSans)
			{

				val is_sub=(i>0)

				val child=gn.childs(san)

				var move_no=child.get_move_no

				if(is_sub)
				{
					pgn+=s"""
						|<font color="#ff0000">($move_no
					""".stripMargin
				}
				else if(child.get_turn=='w')
				{
					val color=if(level==0) "#0000ff" else "#ff0000"
					pgn+=s"""
						|<font color="$color">$move_no</font>
					""".stripMargin
				}

				//val addsan=child.genSan

				val addsan=child.commented_san

				val addlen=addsan.length

				val index=html_pgn_nodes.length

				val sannode=gn.childs(san)

				html_pgn_nodes+=sannode

				var style="padding: 4px;"

				var action=""

				if(cn==sannode)
				{
					style="background-color: #cfffcf; border-style: solid; border-width: 1px; border-color: #000000; border-radius: 10px; padding: 3px;"
					action="editcomment"
				}

				pgn+=s""" <span id="san$index" onmousedown="x='$index';action='$action';" style="$style">$addsan</span> """

				if(is_sub) report_pgn_recursive(gn.childs(san),true,level+1)

				i+=1

			}

			if(numSans>0)
			{
				val main_child=gn.childs(sortedSans(0))

				val sortedSansMain=main_child.sortedSans

				val numSansMain=sortedSansMain.length

				if((numSansMain>0)&&(numSans>1))
				{
					val mains_main_child=main_child.childs(sortedSansMain(0))

					val moveno=mains_main_child.get_move_no

					if(main_child.turn=='b')
					{
						pgn+=s"""
							|<font color="#0000ff">$moveno</font> 
						""".stripMargin
					}
				}

				report_pgn_recursive(main_child,sub,level)
			}
			else if(sub)
			{
				pgn+="""|)</font> 
				""".stripMargin
			}

		}

		val dummy=new board
		dummy.set_from_fen(root.fen)

		if(dummy.getturn==BLACK)
		{
			val fmn=dummy.fullmove_number
			pgn+=s"""
				|<font color="#0000ff">$fmn. ...</font> 
			""".stripMargin
		}
		else if(!HasMoves)
		{
			val fmn=dummy.fullmove_number
			pgn+=s"""
				|<font color="#0000ff">$fmn. </font> 
			""".stripMargin
		}

		if(HasMoves)
		{
			report_pgn_recursive(root,false)
		}

		pgn=pgn.replaceAll(" +"," ")
		pgn=pgn.replaceAll(" +\\)",")")

		pgn
	}

	val PreferredPgnHeaders:List[String]=List("Event","Site","Date","Round","White","Black","Result","TimeControl",
		"Time","TimeZone","Variant","FEN","Termination","Annotator","Opening","ECO")

	def PgnHeaderSortFunc(a:String,b:String):Boolean=
	{
		val ai=PreferredPgnHeaders.indexOf(a)
		val bi=PreferredPgnHeaders.indexOf(b)
		if((ai< 0)&&(bi< 0)) return false
		if(ai< 0) return false
		if(bi< 0) return true
		ai< bi
	}

	def SortedPgnHeaderKeys:List[String]=
	{
		pgn_headers.keys.toList.sortWith(PgnHeaderSortFunc)
	}

	var report_headers_html=""

	def report_pgn_html(cn:gameNode):String=
	{
		pgn_headers+=("FEN"->root.fen)
		pgn_headers+=("Variant"->getvariant)

		report_headers_html=(for(k<-SortedPgnHeaderKeys) yield 
		{
			val v=pgn_headers(k)
			s"""
				|<tr onmousedown="x='edit'; field='$k';">
				|<td><font color="#7f0000">$k</font></td>
				|<td>&nbsp;&nbsp;&nbsp;<font color="#00007f">$v</font></td>
				|</tr>
			""".stripMargin
		}).mkString("\n")

		val move_list_html=report_pgn_move_list_html(cn)

		val term=GetTermination

		s"""
			|<script>
			|var x="";
			|var field="";
			|var action="";
			|</script>
			|<div style="font-family: monospace; font-size: 28px; font-weight: bold;">
			|<table cellpadding="0" cellspacing="0">
			|$report_headers_html
			|</table>
			|<br>
			|$move_list_html
			|$term
			|</div>
		""".stripMargin
	}

	def get_header(key:String):String=
		(if(pgn_headers.contains(key)) pgn_headers(key) else "?")

	def default_log_callback(what:String)
	{
		println(what)
	}

	var log_callback:(String)=>Unit=default_log_callback

	var interrupted=false

	def build_book(pgn:String)
	{

		interrupted=false

		val inc_move_count_old=inc_move_count
		val update_result_old=update_result

		try
		{

			BUILDING=true

			if(add_games) sort_pgn(pgn)

			if(interrupted) return

			val timer=new Timer

			inc_move_count=true
			update_result=true

			val s=split_pgn(pgn)

			var i=0
			for(pgn<-s)
			{

				if(interrupted) return

				current_gameMd5=md5Hex(pgn)

				parse_pgn(pgn)

				val white=get_header("White")
				val black=get_header("Black")

				val variant=get_header("Variant")

				val elapsed="%.1f".format(timer.elapsed)
				val rate="%.3f".format(i/timer.elapsed)

				log_callback((i+1)+s" : added to book $white - $black ( $variant )")
				log_callback(s"elapsed $elapsed , rate $rate")

				i+=1
			}

		}
		finally
		{

			inc_move_count=inc_move_count_old
			update_result=update_result_old

			current_gameMd5=""

			reset

			BUILDING=false

		}

	}

	def game_path(md5Hex:String):String=
		"games/"+variant+"/"+md5Hex+".pgn"

	def get_game(gl:List[String],index:Int)
	{

		val len=gl.length

		if(index>=len) return

		val path=game_path(gl(index))

		val pgn=readFileToString(new File(path),null.asInstanceOf[String])

		parse_pgn(pgn)

	}

	def printableGameLine(md5Hex:String,index:Int,html:Boolean=false):String=
	{
		val path=game_path(md5Hex)

		val dummy=new game
		dummy.parse_pgn(readTxt(path),head_only=true)

		val White=dummy.get_header("White")
		val WhiteElo=dummy.get_header("WhiteElo")
		val Black=dummy.get_header("Black")
		val BlackElo=dummy.get_header("BlackElo")
		val Date=dummy.get_header("Date")
		val Event=dummy.get_header("Event")
		val Round=dummy.get_header("Round")
		val Result=dummy.get_header("Result")

		if(html) return(
			s"""
			|<a name="start"></a>
			|$index. <a href="#start" style="text-decoration: none; font-family: monospace;" onmousedown="setx('$index');">
			|<font color="#007f00">$White ($WhiteElo)</font> 
			|<font color="#7f0000">$Black ($BlackElo)</font> 
			|<font color="#00007f"><b>$Result</b></font> 
			|<font color="#af00af"><i>$Date<i></font> 
			|<font color='#007f7f'><i>$Event #$Round<i></font>
			|</a>
			""".stripMargin.replaceAll("\n","")
			)

		s"$White - $Black $Result ( $Event )"
	}

	def printableGameList(from:Int,to:Int,html:Boolean=false,gl:List[String]):String=
	{

		if(gl.length==0)
		{
			var html_script=""
			if(html)
			{
				html_script="<script>var x='';</script>"
			}
			return html_script+"no games found\n\n"
		}

		val len=gl.length

		var content=s"total of $len games found , listing from "+(from+1)+" :\n\n"

		if(from>=len) return content

		val tom=(if(to>len) len else to)

		val gls=gl.slice(from,tom)

		var i=from
		val l=(for(md5Hex<-gls) yield { i+=1; printableGameLine(md5Hex,i,html=html) }).mkString("\n")+"\n"

		content+=l

		if(html)
		{
			content=content.replaceAll("\n","<br>")

			content="""
			|<script>
			|var x="";
			|function setx(sx)
			|{
				x=sx;
			|}
			|</script>
			|""".stripMargin+
			content
		}

		content

	}

	/////////////////////////////////////////////////////////

	var main_md5=""

	var main_md5_path=""

	var pgn_games=List[String]()

	def build_pgn_game_list
	{

		log_callback(s"building game list for $main_md5")

		val f=new File(main_md5_path)

		if(!(f.exists))
		{
			log_callback(s"manifest $main_md5 does not exists")

			return
		}

		val manifest_str=readFileToString(f)

		val manifest=manifest_str.split("\n")

		var okitems=ArrayBuffer[String]()

		for(item<-manifest)
		{
			val parts=item.split("/")

			val item_variant=parts(0)
			val item_name=parts(1)

			val item_name_parts=item_name.split("\\.")

			val item_md5=item_name_parts(0)

			if(item_variant==getvariant)
			{
				okitems+=item_md5
			}
		}

		pgn_games=okitems.toList

		if(okitems.length==0)
		{
			pgn_games=List[String]()
		}

	}

	var book_games=List[String]()

	def build_book_game_list
	{

		val book_games_str=book.currentPos.games

        book_games=List[String]()

        if(book_games_str!="")
        {
        	book_games=book_games_str.split("_").toList
        }

    }

	/////////////////////////////////////////////////////////

	def sort_pgn(pgn:String)
	{

		mkdir("pgns")

		main_md5=md5Hex(pgn)

		main_md5_path=s"pgns/$main_md5.txt"

		if(new File(main_md5_path).exists)
		{
			log_callback(s"pgn $main_md5 already sorted")

			build_pgn_game_list

			return
		}

		var manifest=ArrayBuffer[String]()

		val a=split_pgn(pgn)

		var counts=Map[String,Int]()

		mkdir("sorted")

		for(delv<-SUPPORTED_VARIANTS)
		{
			val path="sorted"+File.separator+delv+".pgn"

			val f=new File(path)

			if(f.exists)
			{
				//println("deleting "+path)
				f.delete
			}
		}

		for(pgn<-a)
		{

			if(interrupted) return

			parse_pgn(pgn,head_only=true)

			val md5=md5Hex(pgn)

			var current_variant=get_header("Variant")

			if(current_variant=="?")
			{
				current_variant="Standard"
			}

			counts+=(if(counts.contains(current_variant)) (current_variant->(counts(current_variant)+1)) else (current_variant->1))

			log_callback((for(k<-counts.keys.toList.sorted) yield k+" ( "+counts(k)+" )").mkString(" , "))

			mkdirs(List("games",current_variant))

			val manifest_path=s"$current_variant/$md5.pgn"

			val current_game_path=s"games/$manifest_path"

			writeStringToFile(new File(current_game_path),pgn,null.asInstanceOf[String])

			manifest+=manifest_path

			val fw = new FileWriter("sorted/"+current_variant+".pgn", true)

			fw.write(pgn+"\n\n")

			fw.close()

		}

		writeStringToFile(new File(main_md5_path),manifest.mkString("\n"),null.asInstanceOf[String])

		build_pgn_game_list

	}

	def split_pgn(fpgn:String):ArrayBuffer[String]=
	{

		var pgn=fpgn.replaceAll("\r\n","\n")
		pgn=fpgn.replaceAll("\r","")

		val lines=pgn.split("\n"):+""

		var pgn_list=ArrayBuffer[String]()

		val READING_HEAD=0
		val READING_BODY=1

		var status=READING_HEAD

		var content=""

		var i=0
		while(i< lines.length)
		{

			var line=lines(i)

			//println("status"+status+" line "+line)

			val empty=(line=="")

			line=line+"\n"

			if(empty)
			{

				if(status==READING_BODY)
				{

					pgn_list+=content

					content=""

					status=READING_HEAD

				}
				else
				{

					content+="\n"

				}

			}
			else
			{

				if(status==READING_BODY)
				{

					content+=line

				}
				else
				{

					if(line(0)=='[')
					{

						content+=line

					}
					else
					{

						status=READING_BODY

						content+=line

					}

				}

			}

			i+=1
		}

		pgn_list

	}

	def set_from_pgn(pgn:String)
	{
		parse_pgn(pgn)
	}

	def BeginsWith(str:String,what:Character):Boolean=
	{
		if(str==null) return false
		if(str.length==0) return false
		if(str(0)==what) return true
		false
	}

	def EndsWith(str:String,what:Character):Boolean=
	{
		if(str==null) return false
		if(str.length==0) return false
		if(str(str.length-1)==what) return true
		false
	}

	def parse_pgn(set_pgn:String,head_only:Boolean=false)
	{

		PARSING=true

		//if(!head_only) println("\nparsing PGN:\n\n----------------\n"+pgn+"\n----------------\n")

		val READING_HEAD=0
		val READING_BODY=1

		var status=READING_HEAD

		var pgn=set_pgn

		pgn=pgn.replaceAll("\r\n","\n")
		pgn=pgn.replaceAll("\r","")

		val lines=pgn.split("\n")

		var move_list=""

		reset

		var i=0
		var done=false
		while((i< lines.length)&&(!done))
		{

			val line=lines(i)

			val empty=(line=="")

			//println(s"empty $empty line $line")

			if(empty)
			{
				if(status==READING_BODY)
				{
					done=true
				}
			}
			else
			{

				if(status==READING_BODY)
				{
					move_list+=(line+" ")
				}
				else
				{
					if(line(0)=='[')
					{
						val pline=line.replaceAll("\\[|\\]","")

						val parts=pline.split(" +"+'"')

						val key=parts(0)

						val quot='"'

						val value=parts(1).replaceAll(quot+"$","")

						pgn_headers+=(key->value)
					}
					else
					{
						status=READING_BODY
						move_list+=(line+" ")
					}
				}
			}

			i+=1
		}

		for((k,v)<-pgn_headers)
		{
			//println("header "+k+" = "+v)
		}

		if(head_only)
		{
			PARSING=false
			return
		}

		//////////////////////////////////////////////////////////

		if(pgn_headers.contains("Variant"))
		{
			val pgn_variant=pgn_headers("Variant")
			if(pgn_variant!=variant)
			{
				println("Warning: wrong variant, PGN move list could not be parsed.")
				PARSING=false
				return
			}
		}

		if(pgn_headers.contains("FEN"))
		{
			set_from_fen(pgn_headers("FEN"),clear_headers=false)
		}

		val White=get_header("White")
		val Black=get_header("Black")

		val handles=Builder.gss("handles","").split(";").toList

		if(handles.contains(Black)) flip=true
		if(handles.contains(White)) flip=false

		// convert white spaces to space
		move_list=move_list.replaceAll("[\r\n\t]"," ")

		// separate comments
		move_list=move_list.replaceAll("\\}","} ")
		move_list=move_list.replaceAll("\\{"," {")

		// replace multiple spaces with single space
		move_list=move_list.replaceAll(" +"," ")
		// remove leading and trailing spaces
		move_list=move_list.replaceAll("^ | $","")

		// variation opening joined with move
		move_list=move_list.replaceAll("\\( ","(")
		// variation closing joined with move
		move_list=move_list.replaceAll(" \\)",")")
		// separate multiple closings
		move_list=move_list.replaceAll("\\)\\)",") )")

		//println(move_list)

		var moves=move_list.split(" ")

		var commentbuff=""

		var previouswasmove=false

		def parse_moves_recursive()
		{

			while(moves.length>0)
			{

				var move=moves.head

				//println("read move _"+move+"_")

				moves=moves.tail

				if(move.length>0)
				{

					def addcomment
					{
						commentbuff=commentbuff.replaceAll("^\\{|\\}$","")
						if(previouswasmove)
						{
							current_node.comment=commentbuff
						}
						commentbuff=""
						previouswasmove=false
					}

					if(BeginsWith(move,'{'))
					{
						commentbuff=move
						if(EndsWith(move,'}')) addcomment
					}
					else if(EndsWith(move,'}'))
					{
						commentbuff+=" "+move
						addcomment
					}
					else if(commentbuff!="")
					{
						commentbuff+=" "+move
					}
					else
					{

						// remove line numbers, dots from moves
						move=move.replaceAll("^[0-9]*[\\.]*","")

						var open_sub=false

						var close_sub=false

						if(BeginsWith(move,'('))
						{
							open_sub=true
							move=move.substring(1)
						}

						if(EndsWith(move,')'))
						{
							close_sub=true
							move=move.substring(0,move.length-1)
						}

						if(open_sub)
						{
							//println("open sub "+move)
							val save_current_node=current_node
							back
							val m=b.sanToMove(move)
							if(m!=null)
							{
								makeMove(m)
								previouswasmove=true
							} else {
								previouswasmove=false
							}
							parse_moves_recursive()
							current_node=save_current_node
							b.set_from_fen(current_node.fen)
							pos_changed
						}
						else
						{
							val m=b.sanToMove(move)

							//println(b.toPrintable)

							if(m!=null)
							{
								//println("make "+move+" "+m.toAlgeb)
								makeMove(m)
								previouswasmove=true
							}
							else
							{								
								//println("error "+move)
								previouswasmove=false
							}
						}

						if(close_sub)
						{
							//println("sub done")
							return
						}

					}

				}

			}

		}

		parse_moves_recursive()

		tobegin

		PARSING=false

	}
}