package commands

import piece.piece._
import square.square._
import board.board._
import board._
import movetable._
import utils.Parse._
import utils.Dir._
import utils.ClipboardSimple._
import game._
import book._
import settings._
import movetable._

import scala.io.StdIn.readLine

object commands
{

	val EXITCOMMANDS=List("x","gui")

	var g=new game

	var result=""

	def startup
	{

		settings.load

		exec("v "+settings.getvariant)

		g.from_pgn_and_current_line(settings.game_pgn,settings.game_current_line)

		new PosList(settings.get_current_book())

	}

	def exec(commandline:String)
	{

		val tokens=commandline.split(" ")

		var command=tokens(0)

		var rest=tokens.tail.mkString(" ")

		if(tokens.length<=1)
		{
			if(command=="f")
			{
				command="fw"
			}
		}

		if(isInt(command))
		{
			val m=g.b.move_list(command.toInt-1)
			g.makeMove(m)
		}
		else if(command=="r")
		{
			g.reset
		}
		else if(command=="b")
		{
			g.back
		}
		else if(command=="bb")
		{
			g.tobegin
		}
		else if(command=="d")
		{
			g.delete
		}
		else if(command=="flip")
		{
			settings.flip=(!settings.flip)
		}
		else if(command=="fw")
		{
			g.forward
		}
		else if(command=="ff")
		{
			g.toend
		}
		else if(command=="delb")
		{
			val db=tokens(1)
			butils.del_book(db)
			g.reset
			exec("scb default")
		}
		else if(command=="scb")
		{
			val scb=tokens(1)
			settings.set_current_book(scb)
			new PosList(scb)
			g.pos_changed
		}
		else if(command=="cut")
		{
			settings.build_cutoff=parse[Int](tokens(1),0)
			result="Cutoff set to "+settings.build_cutoff+"."
		}
		else if(command=="sort")
		{
			g.sort_pgn(readTxt(tokens(1)))
		}
		else if(command=="lb")
		{
			result=butils.list_books().mkString(" , ")
		}
		else if(command=="s")
		{
			saveTxt("test.pgn",g.report_pgn)
		}
		else if(command=="p")
		{
			g.parse_pgn(readTxt("test.pgn"))
		}
		else if(command=="g")
		{
			g.get_game(g.book_games,parse[Int](tokens(1),1)-1)
		}
		else if(command=="gpgn")
		{
			g.get_game(g.pgn_games,parse[Int](tokens(1),1)-1)
		}
		else if(command=="v")
		{
			val v=rest
			movetable.init(v)
			result=settings.set_variant(v)
			g.reset
			val current_book=settings.get_current_book()
			exec("scb "+current_book)
		}
		else if(command=="build")
		{
			g.build_book(readTxt(tokens(1)))
		}
		else if(command=="a")
		{
			val san=tokens(1)
			val annot=tokens(2)
			val uci=tokens(3)

			g.book.currentPos.annot(san,annot,uci)
			g.book.savePos()
		}
		else if(command=="del")
		{
			val san=tokens(1)

			g.book.currentPos.del(san)
			g.book.savePos()
		}
		else if(command=="cf")
		{
			clipset(g.report_fen)
		}
		else if(command=="fc")
		{
			val fen=clipget
			println("clipfen "+fen)
			g.set_from_fen(fen)
		}
		else if(command=="inc")
		{
			settings.inc_move_count=parse[Boolean](tokens(1),true)
		}
		else if(command=="add")
		{
			settings.add_games=parse[Boolean](tokens(1),true)
		}
		else if(command=="m")
		{
			val san=tokens(1)
			val m=g.b.sanToMove(san)
			if(m==null)
			{
				println("illegal move")
			}
			else
			{
				g.makeMove(m)
			}
		}

	}

	def update
	{

		println(g.b.toPrintable)

		println(g.b.genPrintableMoveList())

		println(g.report_pgn)

		println("\n-> "+g.current_line_pgn+"\n")

		println(g.book.currentPos.toPrintable(html=false))

	}

	def read_input:String=
	{

		val current_book=settings.get_current_book()

		val current_variant=settings.variant

		readLine(s":$result: $current_variant/$current_book> ")

	}

	def loop:String=
	{

		var input=""

		do
		{

			if(input!="") update

			input=read_input

			exec(input)

		}while(!EXITCOMMANDS.contains(input))

		input

	}

	def shutdown
	{

		settings.game_pgn=g.report_pgn

		settings.game_current_line=g.current_line

		settings.save

	}

}

