package board

import piece.piece._
import square.square._
import move._
import movetable.movetable._

import utils.Parse._
import utils.Dir._

import scala.collection.mutable.ArrayBuffer

import scala.util.Random

import settings.settings._

object board
{

	val IS_STALEMATE=0
	val IS_MATE=(-1)

	def STATUS_AS_TEXT(s:Int):String=
	{
		if(s>0) return "position has "+s+" legal moves"
		if(s==IS_STALEMATE) return "position is stalemate"
		if(s==IS_MATE) return "position is mate"
		return "position status unknown"
	}

	var RACING_KINGS_START_FEN="8/8/8/8/8/8/krbnNBRK/qrbnNBRQ w - - 0 1"
	var STANDARD_START_FEN="rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
	var HORDE_START_FEN="rnbqkbnr/pppppppp/8/1PP2PP1/PPPPPPPP/PPPPPPPP/PPPPPPPP/PPPPPPPP w kq - 0 1"

	def gen_chess_960_fen=
	{

		val r=new Random()

		val N=r.nextInt(960)

		var a:Array[Char]=(for(i<-0 to 7) yield ' ').toArray

		val N2=N/4
		val B1=N%4

		a(B1*2+1)='B'

		val N3=N2/4
		val B2=N2%4

		a(B2*2)='B'

		val N4=N3/6
		var Q=N3%6

		var i=0
		while((Q>=0)&&(i< BOARD_SIZE))
		{
			if(a(i)==' ')
			{
				if(Q==0)
				{
					a(i)='Q'
					i=BOARD_SIZE
				}
				else
				{
					Q-=1
				}
			}
			i+=1
		}

		val ka=Array("NN","N N","N  N","N   N"," NN"," N N"," N  N","  NN","  N N","   NN")

		val k=ka(N4)

		i=0
		var kp=0
		while((kp< k.length)&&(i< BOARD_SIZE))
		{
			if(a(i)==' ')
			{
				if(k(kp)=='N')
				{
					a(i)='N'
				}
				kp+=1
			}
			i+=1
		}

		i=0
		var rc=0
		while((i< BOARD_SIZE)&&(rc< 3))
		{
			if(a(i)==' ')
			{
				if(rc==0) a(i)='R'
				if(rc==1) a(i)='K'
				if(rc==2) a(i)='R'
				rc+=1
			}
			i+=1
		}

		val base=a.mkString

		val basel=base.toLowerCase

		s"$basel/pppppppp/8/8/8/8/PPPPPPPP/$base w KQkq - 0 1"

	}

	def start_fen:String=
	{
		if(variant=="Racing Kings") return RACING_KINGS_START_FEN
		if(variant=="Horde") return HORDE_START_FEN
		if(variant=="Chess960") return gen_chess_960_fen
		// default
		STANDARD_START_FEN
	}

	val CASTLE_K=1 << 3
	val CASTLE_Q=1 << 2
	val CASTLE_k=1 << 1
	val CASTLE_q=1 << 0

	val KINGSIDE=0
	val QUEENSIDE=1

	def pawnDir(p:TPiece):Int=
		if(colorOf(p)==BLACK) 1 else -1

	def pawnDistFromBase(p:TPiece,sq:TSquare):Int=
		{
			if(isBlack(p))
			{
				return rankOf(sq)
			}
			else
			{
				return (BOARD_SIZE-1)-rankOf(sq)
			}
		}

	def pawnDistFromProm(p:TPiece,sq:TSquare):Int=
		(BOARD_SIZE-1)-pawnDistFromBase(p,sq)

}

class board
{

	import board._

	//////////////////////////////////////////

	// clone only these

	var turn=WHITE
	var ep_square=NO_SQUARE
	var castling_rights=CASTLE_K|CASTLE_Q|CASTLE_k|CASTLE_q
	var halfmove_clock=0
	var fullmove_number=1

	var rep:Array[TPiece]=emptyrep

	var origRookSquares=Map[Tuple2[TColor,Int],TSquare]()

	var num_checks:Map[TColor,Int]=Map(WHITE->0,BLACK->0)

	//////////////////////////////////////////

	// don't clone these

	var currentSq=0
	var castlingSide=0
	var currentPtr=0
	var hasCapture=false

	var current_move=move()

	var status=0

	//////////////////////////////////////////

	def cclone:board=
	{
		val c=new board

		c.turn=turn
		c.ep_square=ep_square
		c.castling_rights=castling_rights
		c.halfmove_clock=halfmove_clock
		c.fullmove_number=fullmove_number

		Array.copy(rep,0,c.rep,0,BOARD_AREA)

		c.origRookSquares=Map(
			Tuple2(WHITE,KINGSIDE)->origRookSquares(Tuple2(WHITE,KINGSIDE)),
			Tuple2(WHITE,QUEENSIDE)->origRookSquares(Tuple2(WHITE,QUEENSIDE)),
			Tuple2(BLACK,KINGSIDE)->origRookSquares(Tuple2(BLACK,KINGSIDE)),
			Tuple2(BLACK,QUEENSIDE)->origRookSquares(Tuple2(BLACK,QUEENSIDE))
		)

		c.num_checks=Map[TColor,Int](WHITE->num_checks(WHITE),BLACK->num_checks(BLACK))

		c
	}

	//////////////////////////////////////////

	def resetOrigRookSquares
	{
		origRookSquares=Map(
			Tuple2(WHITE,KINGSIDE)->NO_SQUARE,
			Tuple2(WHITE,QUEENSIDE)->NO_SQUARE,
			Tuple2(BLACK,KINGSIDE)->NO_SQUARE,
			Tuple2(BLACK,QUEENSIDE)->NO_SQUARE
		)
	}

	def getturn=turn
	def getinvturn=inverseTurnOf(turn)
	def getpawndir=pawnDir(turn)
	def getpawndistfrombase(sq:TSquare)=pawnDistFromBase(toColor(PAWN,turn),sq)
	def getpawndistfromprom(sq:TSquare)=pawnDistFromProm(toColor(PAWN,turn),sq)

	def to_true_algeb(algeb:String):String=
	{
		if(getvariant=="Chess960") return algeb
		val whitekingalgeb=toAlgeb(whereIsKing(WHITE))
		if(whitekingalgeb=="e1")
		{
			if(algeb=="e1a1") return "e1c1"
			if(algeb=="e1h1") return "e1g1"
		}
		val blackkingalgeb=toAlgeb(whereIsKing(BLACK))
		if(blackkingalgeb=="e8")
		{
			if(algeb=="e8a8") return "e8c8"
			if(algeb=="e8h8") return "e8g8"
		}		
		return algeb
	}

	def to_chess960_algeb(algeb:String):String=
	{
		if(getvariant=="Chess960") return algeb
		val whitekingalgeb=toAlgeb(whereIsKing(WHITE))
		if(whitekingalgeb=="e1")
		{
			if(algeb=="e1c1") return "e1a1"
			if(algeb=="e1g1") return "e1h1"
		}
		val blackkingalgeb=toAlgeb(whereIsKing(BLACK))
		if(blackkingalgeb=="e8")
		{
			if(algeb=="e8c8") return "e8a8"
			if(algeb=="e8g8") return "e8h8"
		}		
		return algeb
	}

	def canCastleToSide(side:Int,c:TColor=getturn):Boolean=
		if(c==WHITE)
		return (castling_rights&(if(side==KINGSIDE) CASTLE_K else CASTLE_Q))!=0
		else
		return (castling_rights&(if(side==KINGSIDE) CASTLE_k else CASTLE_q))!=0

	def canCastle(c:TColor=getturn):Boolean=
		canCastleToSide(KINGSIDE)||canCastleToSide(QUEENSIDE)

	def disableCastleToSide(side:Int,c:TColor=getturn)
	{
		if(c==WHITE)
		castling_rights&=(~(if(side==KINGSIDE) CASTLE_K else CASTLE_Q))
		else
		castling_rights&=(~(if(side==KINGSIDE) CASTLE_k else CASTLE_q))
	}

	def disableCastle(c:TColor=getturn)
	{
		disableCastleToSide(KINGSIDE)
		disableCastleToSide(QUEENSIDE)
	}

	def distFromBase(sq:TSquare,c:TColor=getturn)=
		pawnDistFromBase(c,sq)

	def emptyrep=(for(i<-(1 to BOARD_AREA)) yield NO_PIECE).toArray

	def toRawFen:String=(for(p<-rep) yield toFenChar(p)).toArray.mkString

	def brep=(((toRawFen.grouped(8)).toList).mkString("\n")).replaceAll(" ",".")

	def castlingRightsAsString:String=
		if(castling_rights==0) "-"
		else
		(if((castling_rights&CASTLE_K)!=0) "K" else "")+
		(if((castling_rights&CASTLE_Q)!=0) "Q" else "")+
		(if((castling_rights&CASTLE_k)!=0) "k" else "")+
		(if((castling_rights&CASTLE_q)!=0) "q" else "")

	def set_from_fen(fen:String)
	{

		//println("set from fen "+fen)

		val fen_parts=fen.split(" ")

		if(fen_parts.length>0)
		{
			val posfen=fen_parts(0)

			val posfen_parts=posfen.split("/")

			var i=0
			for(rank<-posfen_parts)
			{
				if(i< BOARD_SIZE)
				{
					var j=0
					for(c<-rank)
					{
						if((c>='0')&&(c<='9'))
						{
							for(k<-1 to (""+c).toInt) 
							{
								if(j< BOARD_SIZE)
								{
									rep(i*BOARD_SIZE+j)=NO_PIECE
									j+=1
								}
							}
						}
						else if(j< BOARD_SIZE)
						{
							rep(i*BOARD_SIZE+j)=fromFenChar(c)
							j+=1
						}
					}
				}
				i+=1
			}
		}

		if(fen_parts.length>1)
		{
			val turnfen=fen_parts(1)

			if(turnfen.length>0)
			{
				val turnchar=turnfen(0)
				turn=WHITE
				if(turnchar=='b') turn=BLACK
			}
		}

		if(fen_parts.length>2)
		{
			val castlefen=fen_parts(2)

			setCastlingRightsFromFen(castlefen)
		}

		if(fen_parts.length>3)
		{
			val epsquarefen=fen_parts(3)

			ep_square=fromAlgeb(epsquarefen)
		}

		if(fen_parts.length>4)
		{
			val halfmovefen=fen_parts(4)

			if(isInt(halfmovefen))
			{
				halfmove_clock=halfmovefen.toInt
			}
		}

		if(fen_parts.length>5)
		{
			val fullmovefen=fen_parts(5)

			if(isInt(fullmovefen))
			{
				fullmove_number=fullmovefen.toInt
			}
		}

		// determine original rook sqaures

		resetOrigRookSquares

		for(c<-List(WHITE,BLACK))
		{
			val r=baseRankOfColor(c)
			var f=0
			var rc=0
			if(!canCastleToSide(QUEENSIDE,c)) rc=1
			val testrook=toColor(ROOK,c)

			while((f< BOARD_SIZE)&&(rc< 2))
			{
				val tsq=fromFileRank(f,r)
				if(rep(tsq)==testrook)
				{
					origRookSquares+=(Tuple2(c,(1-rc))->tsq)
					rc+=1
				}
				f+=1
			}
		}

		num_checks=Map(WHITE->0,BLACK->0)

	}

	def baseRankOfColor(c:TColor):TRank=
		if(c==BLACK) 0 else BOARD_SIZE-1

	def setCastlingRightsFromFen(fen:String)
	{
		castling_rights=0
		for(c<-fen)
		{
			if(c=='K') castling_rights|=CASTLE_K
			if(c=='Q') castling_rights|=CASTLE_Q
			if(c=='k') castling_rights|=CASTLE_k
			if(c=='q') castling_rights|=CASTLE_q
		}
	}

	def report_pattern:String=
	{
		val pattern=(for(rank<-rep.grouped(8)) yield
			{
				var i=0				
				var r=""
				while(i< 8)
				{
					val p=rank(i)
					if(p==NO_PIECE)
					{
						r+="0"
					}
					else
					{
						r+="1"
					}
					i+=1
				}
				r
			}).mkString("\n")+"\n"
		pattern
	}

	def report_trunc_fen:String=
	{
		val fen=report_fen
		val parts=fen.split(" ").toList
		val trunc_fen=parts(0)+" "+parts(1)+" "+parts(2)+" "+parts(3)
		trunc_fen
	}

	def report_fen:String=
	{
		val posfen=(for(rank<-rep.grouped(8)) yield
			{
				var i=0
				var cnt=0
				var r=""
				def addcnt
				{
					if(cnt>0)
					{
						r+="%d".format(cnt)
						cnt=0
					}
				}
				while(i< 8)
				{
					val p=rank(i)
					if(p==NO_PIECE)
					{
						cnt+=1
					}
					else
					{
						addcnt
						r+=toFenChar(p)
					}
					i+=1
				}
				addcnt
				r
			}).mkString("/")

		val turnfen=""+colorLetterOf(turn)

		val castlefen=castlingRightsAsString

		val epfen=toAlgeb(ep_square)

		val halfmovefen=""+halfmove_clock

		val fullmovefen=""+fullmove_number

		s"$posfen $turnfen $castlefen $epfen $halfmovefen $fullmovefen"
	}

	def report_checks:String=
	{
		"w+ "+num_checks(WHITE)+" b+ "+num_checks(BLACK)
	}

	def report_orig_rook_squares:String=
	{
		"orig rook squares: q "+toAlgeb(origRookSquares(Tuple2(BLACK,QUEENSIDE)))+
		" k "+toAlgeb(origRookSquares(Tuple2(BLACK,KINGSIDE)))+
		" Q "+toAlgeb(origRookSquares(Tuple2(WHITE,QUEENSIDE)))+
		" K "+toAlgeb(origRookSquares(Tuple2(WHITE,KINGSIDE)))
	}

	def toPrintable:String=
	{
		
		val fen=report_fen

		val orsqs=report_orig_rook_squares

		val checks=report_checks

		val stinf=s"""
		|variant $variant , $orsqs , $checks
		|fen = $fen
""".stripMargin

		"\n"+brep+"\n"+stinf

	}

	def makeRawMove(algeb:String)
	{

		val m=move(fromalgeb=algeb)

		rep(m.to)=rep(m.from)
		rep(m.from)=NO_PIECE
	}

	def castlingTargetSqK(side:Int,c:TColor=getturn):TSquare=
		if(side==KINGSIDE) fromAlgeb(if(c==WHITE) "g1" else "g8") else fromAlgeb(if(c==WHITE) "c1" else "c8")

	def castlingTargetSqR(side:Int,c:TColor=getturn):TSquare=
		if(side==KINGSIDE) fromAlgeb(if(c==WHITE) "f1" else "f8") else fromAlgeb(if(c==WHITE) "d1" else "d8")

	def makeMove(m:move)
	{

		val rm=toRichMove(m)

		ep_square=NO_SQUARE

		var do_promote=true

		if(rm.castling_move)
		{

			rep(rm.from)=NO_PIECE
			rep(rm.to)=NO_PIECE

			val kingto=castlingTargetSqK(rm.castling_side)

			rep(kingto)=toColor(KING,turn)

			val rookto=castlingTargetSqR(rm.castling_side)

			rep(rookto)=toColor(ROOK,turn)

			disableCastle()

		}
		else
		{

			rep(rm.to)=rep(rm.from)

			if((rm.capture)&&(variant=="Atomic"))
			{
				rep(rm.to)=NO_PIECE

				for(esq<-kingAdjSqs(rm.to))
				{
					if(typeOf(rep(esq))!=PAWN)
					{
						rep(esq)=NO_PIECE
					}
				}

				do_promote=false
			}

			if((rm.prom_piece!=NO_PIECE)&&(do_promote))
			{
				rep(rm.to)=toColor(rm.prom_piece,turn)
			}

			rep(rm.from)=NO_PIECE

			if(rm.pawn_double_push)
			{
				ep_square=rm.pawn_passing_square
			}

			val algeb=rm.toAlgeb

			if(rm.ep_capture)
			{
				rep(rm.ep_clear_square)=NO_PIECE
			}

			// check if rook move/capture or king move disables castling
			for(cc<- List(WHITE,BLACK))
				{
					for(side<- List(KINGSIDE,QUEENSIDE))
					{
						val orsq=origRookSquares(Tuple2(cc,side))

						if(orsq!=NO_SQUARE)
						{
							if(rep(orsq)==NO_PIECE)
							{
								disableCastleToSide(side,cc)
							}
						}
					}
				}

			if(rm.from_piece==toColor(KING,turn))
			{
				disableCastle()
			}
		}

		///////////////////////////////////////////

		turn=inverseTurnOf(turn)

		///////////////////////////////////////////

		if(isInCheck) num_checks+=(turn->(num_checks(turn)+1))

		///////////////////////////////////////////

		// take care of move counts

		if((typeOf(rm.from_piece)==PAWN)||(rm.to_piece!=NO_PIECE)||(rm.ep_capture))
		{
			halfmove_clock=0
		}
		else
		{
			halfmove_clock+=1
		}

		if(turn==WHITE)
		{
			fullmove_number+=1
		}

	}

	def kingAdjSqs(sq:TSquare):ArrayBuffer[TSquare]=
	{
		var ptr=move_table_ptrs(Tuple2(sq,KING))

		var sqs=ArrayBuffer[TSquare]()

		while(!move_table(ptr).end_piece)
		{
			sqs+=move_table(ptr).to
			ptr+=1
		}

		sqs
	}

	def isSqFree(sq:TSquare,c:TColor=getturn):Boolean=
	{
		val p=rep(sq)

		if(p==NO_PIECE) return true

		if(colorOf(p)==inverseColorOf(c)) return true

		false
	}

	def isSqKingAdj(sq:TSquare,c:TColor):Boolean=
	{
		val testking=toColor(KING,c)

		for(sq<-kingAdjSqs(sq))
		{
			if(rep(sq)==testking) return true
		}

		false
	}

	def isAlgebLegal(algeb:String,test:Boolean=false):Boolean=
	{
		val m=move(fromalgeb=algeb)

		if(m.from==NO_SQUARE) return false
		if(m.to==NO_SQUARE) return false

		val dummy=this.cclone

		dummy.initMoveGen

		dummy.currentSq=m.from

		if(!dummy.setCurrentPtr(m.from,rep(m.from))) return false

		var ok=true

		if(test) { println("testing moves"); println(toPrintable) }

		while(dummy.nextLegalMove&&ok)
		{
			ok=(dummy.currentSq==m.from)
			if(ok)
			{
				val calgeb=dummy.current_move.toAlgeb
				if(test) println("calgeb "+calgeb+" algeb "+algeb)
				if(calgeb==algeb) return true
			}
		}

		dummy.currentSq=NO_SQUARE

		if(test) println("testing castling")

		while(dummy.nextLegalMove)
		{
			val calgeb=dummy.current_move.toAlgeb
			if(test) println("calgeb "+calgeb+" algeb "+algeb)
			if(calgeb==algeb) return true
		}

		false
	}

	def pieceMovesToSq(p:TPiece,sq:TSquare):ArrayBuffer[move]=
	{
		val a=ArrayBuffer[move]()

		val test_piece=toColor(p,getinvturn)

		val dummy=this.cclone

		dummy.rep(sq)=test_piece

		dummy.turn=inverseTurnOf(turn)

		dummy.initMoveGen

		dummy.currentSq=sq

		dummy.setCurrentPtr(sq,test_piece)

		var ok=true

		//println("p "+toFenChar(p)+" sq "+toAlgeb(sq))

		while(dummy.nextMove&&ok)
		{
			ok=(dummy.currentSq==sq)

			if(ok)
			{
				val cm=dummy.current_move

				//println(cm.toAlgeb+" cm.to "+toFenChar(rep(cm.to)))

				if((rep(cm.to)==p))
				{

					val m=toRichMove(move(from=cm.to,to=cm.from))

					a+=m

				}
			}
		}

		a
	}

	def whereIsKing(c:TColor=getturn):TSquare=
	{
		val i=rep.indexOf(toColor(KING,c))
		if(i< 0) return NO_SQUARE
		return i
	}

	def setCurrentPtr(sq:TSquare,p:TPiece):Boolean=
	{
		val ptr:TMoveTablePtr=Tuple2(sq,p)
		if(move_table_ptrs.contains(ptr))
		{
			currentPtr=move_table_ptrs(ptr)
			return true
		}
		return false
	}

	def nextSq
	{
		while(currentSq< BOARD_AREA)
		{
			if(setCurrentPtr(currentSq,rep(currentSq))) return

			currentSq+=1
		}
	}

	def initMoveGen
	{

		currentSq=0
		castlingSide=0
		nextSq

		if(variant=="Antichess")
		{

			hasCapture=false

			while(nextLegalMove&&(!hasCapture))
			{
				val rm=toRichMove(current_move)

				hasCapture=rm.capture
			}

			currentSq=0
			castlingSide=0
			nextSq

		}

	}

	def hasMaterial(c:TColor):Boolean=
	{
		var found=false

		var sq=0
		while((sq< BOARD_AREA)&&(!found))
		{
			val p=rep(sq)

			if((p!=NO_PIECE)&&(colorOf(p)==c)) found=true

			sq+=1
		}

		found
	}

	def clear
	{
		for(sq<-0 to BOARD_AREA-1) rep(sq)=NO_PIECE
	}

	def revert
	{
		for(sq<-0 to BOARD_AREA-1) rep(sq)=toInvColor(rep(sq))
	}

	def isInGlobalCheck(c:TColor):Boolean=
	{

		if(variant=="Racing Kings")
		{
			val oksq=whereIsKing(inverseColorOf(c))

			if(rankOf(oksq)!=0) return false

			val ksq=whereIsKing(c)

			val r=rankOf(ksq)

			if(r>1) return true

			if(r==0) return false

			val f=fileOf(ksq)

			for(i<- -1 to 1)
			{
				val tf=f+i

				if(fileOk(tf))
				{
					val testsq=fromFileRank(tf,0)

					if(
						(!isInCheckSq(testsq,allow_global=false))
						&&
						(isSqFree(testsq))
					) return false
				}
			}

			return true
		}

		if(variant=="Horde")
		{
			if(!hasMaterial(c)) return true
		}

		if(variant=="Antichess")
		{
			if(!hasMaterial(inverseColorOf(c))) return true
		}

		if(variant=="Atomic")
		{
			if(isExploded(c)) return true
		}

		if(variant=="Three-check")
		{
			val nc=num_checks(c)

			if(nc>=3) return true
		}
		
		if(variant=="King of the Hill")
		{
			val oppksq=whereIsKing(inverseColorOf(c))
			
			if(isCentralSquare(oppksq)) return true
		}

		false
	}

	def isInCheckSqCol(sq:TSquare,c:TColor,allow_global:Boolean=true):Boolean=
	{

		if(allow_global)
		{

			if(isInGlobalCheck(c)) return true

		}

		if(variant=="Antichess") return false

		if(sq==NO_SQUARE)
		{
			//println("warning: checking no square for check")
			return false
		}

		if(variant=="Atomic")
		{
			if(isSqKingAdj(sq,inverseColorOf(c))) return false
		}

		var checking_pieces=List(KING,QUEEN,ROOK,BISHOP,KNIGHT)
		for(test_piece_type<-checking_pieces)
		{
			val test_color=inverseColorOf(c)
			val test_piece=toColor(test_piece_type,test_color)

			var ptr=move_table_ptrs(Tuple2(sq,test_piece))

			while(!move_table(ptr).end_piece)
			{
				val cm=move_table(ptr)
				val cto=cm.to
				val cp=rep(cto)

				if(cp==NO_PIECE)
				{
					ptr+=1
				}
				else if(colorOf(cp)==c)
				{
					ptr=cm.next_vector
				}
				else
				{
					if(cp==test_piece)
					{
						return true
					}
					else
					{
						ptr=cm.next_vector
					}
				}
			}
		}

		// pawn checks

		val pd=pawnDir(c)

		val f=fileOf(sq)
		val tr=rankOf(sq)+pd

		for(i<-List(-1,1))
		{
			val tf=f+i
			if(fileRankOk(tf,tr))
			{
				val tsq=fromFileRank(tf,tr)
				if(rep(tsq)==toColor(PAWN,inverseColorOf(c))) return true
			}
		}

		return false
	}

	def isInCheckSq(sq:TSquare,allow_global:Boolean=true):Boolean=isInCheckSqCol(sq,turn,allow_global)

	def isInCheckCol(c:TColor,allow_global:Boolean=true):Boolean=
	{
		isInCheckSqCol(whereIsKing(c),c,allow_global)
	}

	def isInCheck:Boolean=
	{
		isInCheckSqCol(whereIsKing(),turn)
	}

	def isOppInCheck:Boolean=isInCheckSqCol(whereIsKing(getinvturn),getinvturn)

	// determine move properties
	def toRichMove(m: move):move=
	{
		var rm=move(from=m.from,to=m.to,prom_piece=m.prom_piece)

		val algeb=rm.toAlgeb

		rm.from_piece=rep(m.from)
		rm.to_piece=rep(m.to)

		val from_file=fileOf(rm.from)
		val from_rank=rankOf(rm.from)
		val to_file=fileOf(rm.to)
		val to_rank=rankOf(rm.to)

		if((typeOf(rm.from_piece)==KING)&&(rm.to_piece==toColor(ROOK,turn)))
		{
			rm.castling_move=true
			rm.castling_side=if(fileOf(rm.from)< fileOf(rm.to)) KINGSIDE else QUEENSIDE
		}

		if((rm.to_piece!=NO_PIECE)&&(!rm.castling_move))
		{
			rm.capture=true
		}

		if(typeOf(rm.from_piece)==KING)
		{
			rm.king_move=true
		}

		if(typeOf(rm.from_piece)==PAWN)
		{

			rm.pawn_move=true
			
			if(fileOf(rm.from)!=fileOf(rm.to))
			{
				rm.capture=true
				rm.pawn_capture=true

				if(rm.to==ep_square)
				{
					rm.ep_capture=true
					rm.ep_clear_square=fromFileRank(to_file,from_rank)
				}
			}
			else
			{
				rm.pawn_push=true

				val push=rankOf(rm.to)-rankOf(rm.from)

				if(Math.abs(push)==1)
				{
					rm.pawn_single_push=true
				}
				else
				{
					rm.pawn_double_push=true
					
					rm.pawn_passing_square=fromFileRank(from_file,from_rank+push/2)
				}
			}

			if(getpawndistfromprom(rm.from)==1)
			{
				rm.pawn_promotion=true
			}

		}

		rm
	}

	def isExploded(c:TColor):Boolean=
	{
		val ksq=whereIsKing(c)

		if(ksq==NO_SQUARE) return true

		return false
	}

	def nextLegalMove:Boolean=
	{

		if(variant=="Racing Kings")
		{
			val wksq=whereIsKing(WHITE)
			val rw=rankOf(wksq)

			val bksq=whereIsKing(BLACK)
			val rb=rankOf(bksq)

			if((rw==0)&&(rb==0)) return false
		}

		while(nextMove)
		{
			val dummy=this.cclone

			dummy.makeMove(current_move)

			val meincheck=dummy.isInCheckCol(turn)
			val oppinlocalcheck=dummy.isInCheckCol(inverseTurnOf(turn),allow_global=false)

			if(variant=="Antichess")
			{
				val rm=toRichMove(current_move)

				val ok=((!hasCapture)||(rm.capture))

				if((!meincheck)&&ok) return true

				if((!dummy.hasMaterial(inverseTurnOf(turn)))&&ok) return true
			}
			else
			{
				if(!meincheck)
				{
					if(variant=="Racing Kings")
					{
						if(!oppinlocalcheck) return true
					}
					else
					{
						return true
					}
				}

				if(variant=="Atomic")
				{
					if(dummy.isExploded(getinvturn)&&(!dummy.isExploded(turn))) return true
				}
			}
		}

		return false
	}

	def nextMove:Boolean=
	{

		while(currentSq< BOARD_AREA)
		{

			var piece_done=false
			while((!move_table(currentPtr).end_piece)&&(!piece_done))
			{

				val m=move_table(currentPtr).cclone

				val to=m.to

				if(to!=NO_SQUARE)
				{

					val from_piece=rep(currentSq)

					val to_piece=rep(to)

					val to_piece_color=colorOf(to_piece)

					if(colorOf(from_piece)==turn)
					{
						if(to_piece==NO_PIECE)
						{

							// take care of ep capture
							var set_ep_capture=false
							if(m.pawn_capture)
							{
								if(to==ep_square)
								{
									set_ep_capture=true
								}
							}
							m.ep_capture=set_ep_capture

							// empty square
							// only allowed if not pawn capture or ep capture
							if((!m.pawn_capture)||m.ep_capture)
							{
								// pawn double push only possible if passing square empty
								if(
									(!m.pawn_double_push)
									||
									(m.pawn_double_push&&(rep(m.pawn_passing_square)==NO_PIECE))
									)
								{
									current_move=m
									currentPtr+=1
									return true
								}
								else
								{
									currentPtr+=1
								}
							}
							else
							{
								currentPtr+=1
							}
						}
						else if(to_piece_color==turn)
						{
							// bumps into own piece
							currentPtr=m.next_vector
						}
						else
						{
							m.capture=true
							// capture
							// but pawn cannot capture forward
							if(!m.pawn_push)
							{
								current_move=m
								// in case of promotion don't go to next vector
								if(m.pawn_capture)
								{
									currentPtr+=1
								}
								else
								{
									currentPtr=m.next_vector
								}
								return true
							}
							else
							{
								currentPtr=m.next_vector
							}
						}

					}
					else
					{
						piece_done=true
					}
				}
				else
				{
					piece_done=true
				}

			}

			currentSq+=1
			nextSq

		}

		// look at castling

		// no castling in Antichess

		if(variant=="Antichess") return false

		val ksq=whereIsKing()

		// no king no castle
		if(ksq==NO_SQUARE)
		{
			return false
		}

		// castling only possible on base rank
		if(distFromBase(ksq)!=0)
		{
			return false
		}

		while(castlingSide< 2)
		{

			var kf=fileOf(ksq)
			val kr=rankOf(ksq)

			if(canCastleToSide(castlingSide))
			{
				// look for rook
				val dir=if(castlingSide==KINGSIDE) 1 else -1
				val testrook=toColor(ROOK,turn)

				// check if squares between king and rook are empty
				kf+=dir
				var ok=true
				while(fileOk(kf)&&(rep(fromFileRank(kf,kr))!=testrook)&&ok)
				{
					val psq=fromFileRank(kf,kr)

					ok=(rep(psq)==NO_PIECE)

					kf+=dir
				}

				// now kf should hold the closest rook's file if a rook is found
				var initial_test_ok=(fileOk(kf)&&ok)

				var orsq=origRookSquares(Tuple2(turn,castlingSide))
				var arsq=fromFileRank(kf,kr)

				// check if this rook is the one to be used for castling
				if(initial_test_ok)
				{
					if((arsq!=orsq)||(orsq==NO_SQUARE))
					{
						initial_test_ok=false
					}
				}

				if(initial_test_ok)
				{
					// squares between king and rook empty
					// look for checks

					val tsqk=castlingTargetSqK(castlingSide)
					val tsqr=castlingTargetSqR(castlingSide)

					// no castling if king target square is in check
					if(isInCheckSq(tsqk))
					{
						castlingSide+=1
					}
					// or if it is not empty
					else if( (rep(tsqk)!=NO_PIECE) && (tsqk!=ksq) && (tsqk!=orsq) )
					{
						castlingSide+=1
					}
					// or if the rook target square not empty
					else if( (rep(tsqr)!=NO_PIECE) && (tsqr!=ksq) && (tsqr!=orsq) )
					{
						castlingSide+=1
					}
					else
					{

						val step=if(tsqk>ksq) 1 else -1

						var csqk=ksq

						// look if passing squares including original are not in check and empty
						var pcok=true
						while((csqk!=tsqk)&&pcok)
						{
							pcok=(!isInCheckSq(csqk))
							// check empty only if not king's current square
							if(csqk!=ksq)
							{
								if( (rep(csqk)!=NO_PIECE) && (csqk!=ksq) && (csqk!=orsq) )
								{
									pcok=false
								}
							}
							csqk+=step
						}

						if(pcok)
						{
							// everything is ok
							val m=move(from=ksq,to=arsq,castling_move=true,castling_side=castlingSide)
							castlingSide+=1
							current_move=m
							return true
						}
						else
						{
							castlingSide+=1
						}

					}
				}
				else
				{
					castlingSide+=1
				}
			}
			else
			{
				castlingSide+=1
			}

		}

		return false

	}

	var move_list=ArrayBuffer[move]()

	def genMoveList
	{
		genMoveListInner

		move_list=move_list.sortWith(toSan(_)< toSan(_))
	}

	def genMoveListInner
	{

		move_list=ArrayBuffer[move]()

		initMoveGen
	
		while(nextLegalMove)
		{
			move_list+=current_move.cclone
		}

		val no_moves=move_list.length

		if(no_moves>0)
		{
			status=no_moves
			return
		}

		if(isInCheck)
		{
			status=IS_MATE
			return
		}

		if(variant=="Antichess")
		{
			if(!hasMaterial(turn))
			{
				status=IS_MATE
				return
			}
		}

		status=IS_STALEMATE

	}

	def statusOf(m:move):String=
	{
		val dummy=this.cclone

		dummy.makeMove(m)

		val check=dummy.isInCheckCol(getinvturn)

		dummy.initMoveGen

		val haslegal=dummy.nextLegalMove

		if(check)
		{
			if(haslegal) return "+"
			return "#"
		}

		if(haslegal) return ""

		if(variant=="Antichess")
		{
			if(!dummy.hasMaterial(inverseTurnOf(turn))) return "#"
		}

		return "="
	}

	def qualifyMoveList
	{
		var i=0
		for(m<-move_list)
		{
			val st=statusOf(m)
			move_list(i).status=st
			i+=1
		}
	}

	def printableMoveList(html:Boolean=false):String=
	{

		val statt=STATUS_AS_TEXT(status)

		if(!html)
		{
			var i=0
			val mlstr=(for(m<-move_list) yield
			{
				i+=1
				"%d : %s".format(i,toSan(m))
			}).mkString(" , ")

			return statt+(if(status>0) " , namely : "+mlstr else "")+"\n"
		}

		val td="""<td align="center">"""

		var i=0
		val items=(for(m<-move_list) yield
		{
			i+=1
			val san=toSan(m)
			val algeb=m.toAlgeb
			s"""
				|<tr>
				|$td
				|<font size="3" color="#000000">$i.</font>
				|</td>
				|$td
				|<a name="dummy"></a>
				|<a href="#dummy" onmousedown="x='$san';" style="text-decoration: none;">
				|<font size="3" color="#007f00"><b>$san<b></font>
				|</a>
				|</td>
				|$td
				|<font size="3" color="#00007f">$algeb</font>
				|</td>
				|</tr>
			""".stripMargin
		}).mkString("\n")

		val script=
		s"""
			|<script>
			|var x="";
			|</script>
		""".stripMargin

		val content=if(i>0) s"""
			|$script
			|<table cellpadding="3" cellspacing="3">
			|<tr><td colspan="3">$statt</td></tr>
			|<tr>
			|$td <i>No.</i></td>
			|$td <i>san</i></td>
			|$td <i>algeb</i></td>
			|</tr>
			|
			|$items
			|
			|</table>
		""".stripMargin else s"""
			|$script
			|<table cellpadding="3" cellspacing="3">
			|<tr><td>
			|$statt
			|</td></tr>
			|</table>
		""".stripMargin

		content
	}

	def genPrintableMoveList(html:Boolean=false):String=
	{
		genMoveList
		qualifyMoveList
		printableMoveList(html)
	}

	def reset
	{
		set_from_fen(start_fen)
	}

	def toSan(m:move):String=
	{
		val rawSan=toRawSan(m)

		val status=statusOf(m)

		rawSan+status
	}

	def toRealAlgebMove(m:move):move=
	{
		val rm=toRichMove(m)

		if(!rm.castling_move) return m

		m.to=castlingTargetSqK(rm.castling_side,turn)

		m
	}

	def toRawSan(m:move):String=
	{
		val rm=toRichMove(m)

		if(rm.pawn_move||rm.king_move)
		{
			return rm.toSan()
		}

		val a=pieceMovesToSq(rm.from_piece,rm.to)

		val san=rm.toSan("")
		var sancnt=0

		val sanf=rm.toSan("f")
		var sanfcnt=0

		val sanr=rm.toSan("r")
		var sanrcnt=0

		val sanfr=rm.toSan("fr")

		//println("disamb "+sanfr+" san "+san+" sanf "+sanf+" sanr "+sanr)

		for(tm<-a)
		{
			val tmr=toRichMove(tm)
			tmr.from_piece=rm.from_piece

			val tsan=tmr.toSan("")
			val tsanf=tmr.toSan("f")
			val tsanr=tmr.toSan("r")
			val tsanfr=tmr.toSan("fr")

			if(san==tsan) sancnt+=1
			if(sanf==tsanf) sanfcnt+=1
			if(sanr==tsanr) sanrcnt+=1

			//println("tsanfr "+tsanfr+" tsan "+tsan+" "+sancnt+" tsanf "+tsanf+" "+sanfcnt+" tsanr "+tsanr+" "+sanrcnt)
		}

		if(sancnt<=1) return san
		if(sanfcnt<=1) return sanf
		if(sanrcnt<=1) return sanr
		
		sanfr
	}

	def classifySan(san:String):Tuple3[String,TPiece,String]=
	{

		val sans=san.replaceAll("x| ","")

		val parts=sans.split("=")

		var prom_piece=NO_PIECE

		if(parts.length>1)
		{
			val prom_part=parts(1)
			if(prom_part.length>0)
			{
				prom_piece=fromFenChar(prom_part(0))
			}
		}

		val san_part=parts(0)

		var ok=true

		var i=0

		var sanclass=""

		while((i< Math.min(5,san_part.length))&&ok)
		{

			val c=san_part(i)

			if((c>='A')&&(c<='Z'))
			{
				sanclass+="P"
			}
			else if((c>='a')&&(c<='h'))
			{
				sanclass+="f"
			}
			else if((c>='1')&&(c<='8'))
			{
				sanclass+="r"
			}
			else
			{
				ok=false
			}

			i+=1

			//println(i+""+sanclass)

		}

		Tuple3(sanclass,prom_piece,sans)

	}

	def sanToMove(sanfull:String):move=
	{

		val s3=if(sanfull.length>=3) sanfull.substring(0,3) else sanfull
		val s5=if(sanfull.length>=5) sanfull.substring(0,5) else sanfull

		if((s3=="O-O")||(s5=="O-O-O"))
		{
			val kingSq=whereIsKing()
			val side=(if(s5=="O-O-O") QUEENSIDE else KINGSIDE)
			val rookSq=origRookSquares(Tuple2(turn,side))
			val fromalgeb=toAlgeb(kingSq)+toAlgeb(rookSq)
			val m=move(fromalgeb=fromalgeb)
			return m
		}

		val classify=classifySan(sanfull)

		val sanclass=classify._1
		val prom_piece=classify._2
		val san=classify._3

		var to=NO_SQUARE
		var from_piece=NO_PIECE

		var from_file=(-1)
		var from_rank=(-1)

		//println("class "+sanclass)

		if(sanclass=="fr")
		{
			// pawn push
			from_piece=PAWN
			to=fromAlgeb(san.substring(0,2))
		}
		else if(sanclass=="ffr")
		{
			// pawn capture
			from_piece=PAWN
			to=fromAlgeb(san.substring(1,3))
			from_file=algebFileToFile(san(0))
		}
		else if(sanclass=="Pfr")
		{
			from_piece=fromFenChar(san(0))
			to=fromAlgeb(san.substring(1,3))
		}
		else if(sanclass=="Pffr")
		{
			from_piece=fromFenChar(san(0))
			to=fromAlgeb(san.substring(2,4))
			from_file=algebFileToFile(san(1))
		}
		else if(sanclass=="Prfr")
		{
			from_piece=fromFenChar(san(0))
			to=fromAlgeb(san.substring(2,4))
			from_rank=algebRankToRank(san(1))
		}
		else if(sanclass=="Pfrfr")
		{
			from_piece=fromFenChar(san(0))
			to=fromAlgeb(san.substring(3,5))
			from_file=algebFileToFile(san(1))
			from_rank=algebRankToRank(san(2))
		}
		else
		{
			return null
		}

		from_piece=toColor(from_piece,turn)

		var from=NO_SQUARE

		if(typeOf(from_piece)==PAWN)
		{

			if(from_file==(-1))
			{
				// pawn push
				from_file=fileOf(to)
				from_rank=rankOf(to)-getpawndir

				if(!rankOk(from_rank))
				{
					return null
				}

				from=fromFileRank(from_file,from_rank)

				if(rep(from)!=from_piece)
				{
					// double push
					from_rank-=getpawndir

					if(!rankOk(from_rank))
					{
						return null
					}

					from=fromFileRank(from_file,from_rank)

					if(rep(from)!=from_piece)
					{
						// pawn not found
						return null
					}

					return move(from=from,to=to,prom_piece=prom_piece)
				}
				else
				{
					return move(from=from,to=to,prom_piece=prom_piece)
				}
				
			}
			else
			{
				// pawn capture
				from_rank=rankOf(to)-getpawndir

				from=fromFileRank(from_file,from_rank)

				return move(from=from,to=to,prom_piece=prom_piece)
			}
		}
		else
		{
			val a=pieceMovesToSq(from_piece,to)

			for(tm<-a)
			{
				val fileok=(from_file==(-1))||(from_file==fileOf(tm.from))
				val rankok=(from_rank==(-1))||(from_rank==rankOf(tm.from))

				if(fileok&&rankok)
				{
					return move(from=tm.from,to=tm.to,prom_piece=prom_piece)
				}
			}
		}

		null
	}

}