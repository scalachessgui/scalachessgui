package guiboard

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

import utils.Resource

import square.square._
import piece.piece._

import board._

import commands.commands._

import move._

import components._

import settings._

import utils.Parse._

import builder._
import data._

class guiboard(
	val piece_size:Int=65
	)
{

	val piece_factor=0.8
	val padding_factor=(1-piece_factor)/2

	val piece_padding=(padding_factor*piece_size).toInt

	val fontpath="fonts/"+Builder.gss("chessfont#selected","MERIFONTNEW.TTF")

	val chess_font=Font.loadFont(Resource.asStream(fontpath),piece_size*piece_factor)

	val small_chess_font=Font.loadFont(Resource.asStream(fontpath),22)

	val rooth=new HBox()

	val material=Builder.gss("material#selected","wood")

	rooth.setBackground(new Background(new BackgroundImage(
			new Image(Resource.asStream(s"images/$material.jpg")),
			BackgroundRepeat.REPEAT,BackgroundRepeat.REPEAT,BackgroundPosition.CENTER,BackgroundSize.DEFAULT
			)))

	val root=new Group()

	rooth.getChildren().add(root)

	val board_size=piece_size*BOARD_SIZE

	var margin=Builder.gss("boardmargin","30.0").toDouble.toInt
	var showcoords=Builder.gss("showcoords","true").toBoolean

	val canvas_size=board_size+2*margin

	val canvas_width=canvas_size
	val canvas_height=canvas_size

	var b=new board

	case class Layer(opacity:Double=1.0)
	{
		val canvas=new Canvas(canvas_width,canvas_height)

		canvas.setOpacity(opacity)

		val gc=canvas.getGraphicsContext2D()
	}

	var translit_light=Map[Char,Char]()
	var translit_dark=Map[Char,Char]()

	def init_translit
	{
		translit_light+=(' '->' ');
        translit_light+=('P'->'p');
        translit_light+=('N'->'n');
        translit_light+=('B'->'b');
        translit_light+=('R'->'r');
        translit_light+=('Q'->'q');
        translit_light+=('K'->'k');
        translit_light+=('p'->'o');
        translit_light+=('n'->'m');
        translit_light+=('b'->'v');
        translit_light+=('r'->'t');
        translit_light+=('q'->'w');
        translit_light+=('k'->'l');
		
        translit_dark+=(' '->'+');
        translit_dark+=('P'->'P');
        translit_dark+=('N'->'N');
        translit_dark+=('B'->'B');
        translit_dark+=('R'->'R');
        translit_dark+=('Q'->'Q');
        translit_dark+=('K'->'K');
        translit_dark+=('p'->'O');
        translit_dark+=('n'->'M');
        translit_dark+=('b'->'V');
        translit_dark+=('r'->'T');
        translit_dark+=('q'->'W');
        translit_dark+=('k'->'L');
	}

	init_translit

	val translits=Map(DARK->translit_dark,LIGHT->translit_light)

	var flip:Boolean=false

	var setup:Boolean=false

	var setup_piece=NO_PIECE

    def true_index(index:Int):Int=
    {
        if(flip) (7-index) else index
    }
    
    def true_index_inv(index:Int):Int=
    {
        (7-true_index(index))
    }
    
    def px_to_index(px:Int):Int=
    {
        true_index((px-margin)/piece_size)
    }

    def default_manual_move_made_callback(m:move,san:String)
    {
    	println("manual move made algeb "+m.toAlgeb+" san "+san)
    }

    var manual_move_made_callback:(move,String)=>Unit=default_manual_move_made_callback

    def manual_move_made(m:move)
    {

    	val san=b.toSan(m)

    	b.makeMove(m)

        draw_board

        manual_move_made_callback(m,san)

    }

	class myMouseEventHandler extends EventHandler[MouseEvent]
	{

		var is_drag_going:Boolean=false
	    var drag_from:TSquare=NO_SQUARE
	    var drag_to:TSquare=NO_SQUARE
	    var drag_dx:Int=0
	    var drag_dy:Int=0
	    var drag_piece:TPiece=NO_PIECE
	    var mouse_released:Boolean=false

		def handle(mouseEvent:MouseEvent)
		{
			var x:Int=mouseEvent.getX().toInt
            var y:Int=mouseEvent.getY().toInt
            var etype:String=mouseEvent.getEventType().toString()

            //println("x "+x+" y "+y+" e "+etype)

            if(etype=="MOUSE_RELEASED")
            {
                if(is_drag_going)
                {

                	var do_draw=true
                    
                    is_drag_going=false

                    b.rep(drag_from)=drag_piece
                    
                    drag_to=fromFileRank(px_to_index(x),px_to_index(y))
                    
                    if(drag_to!=NO_SQUARE)
                    {
                        
                        if(setup)
                        {
                            
                            if(drag_from!=drag_to)
                            {
                    
                                mouse_released=true

                                b.rep(drag_from)=NO_PIECE
                                b.rep(drag_to)=drag_piece

                                draw_board
                            
                            }
                            
                        }
                        else
                        {
                            
                            var m=move(from=drag_from,to=drag_to)

                            def is_legal(m:move):Boolean=
                            {

                            	b.initMoveGen

	                            var legal:Boolean=false

	                            while(b.nextLegalMove&&(!legal))
	                            {
	                            	//println(b.current_move.toAlgeb+" = "+m.toAlgeb)
	                            	if(
	                            		(b.current_move.from==m.from)&&
	                            		(b.current_move.to==m.to)&&
	                            		(b.current_move.prom_piece==m.prom_piece)
	                            	)
	                            	{
	                            		legal=true
	                            	}
	                            	//println("legal "+legal)
	                            }

	                            legal

                        	}
                        
                            if(is_legal(m))
                            {
                                manual_move_made(m)
                            }
                            else
                            {
                                m=move(from=drag_from,to=drag_to,prom_piece=fromFenChar('q'))

                                if(is_legal(m))
                                {

                                	val dialog=if(settings.variant=="Antichess") "promotedialoganti" else "promotedialog"

                                	def handler(ev:builder.MyEvent)
                                	{
                                		if(ev.id=="promqueen")
                                		{
                                			m.prom_piece=fromFenChar('q')
                                			Builder.closeStage(dialog)
                                			manual_move_made(m)
                                		}

                                		if(ev.id=="promrook")
                                		{
                                			m.prom_piece=fromFenChar('r')
                                			Builder.closeStage(dialog)
                                			manual_move_made(m)
                                		}

                                		if(ev.id=="prombishop")
                                		{
                                			m.prom_piece=fromFenChar('b')
                                			Builder.closeStage(dialog)
                                			manual_move_made(m)
                                		}

                                		if(ev.id=="promknight")
                                		{
                                			m.prom_piece=fromFenChar('n')
                                			Builder.closeStage(dialog)
                                			manual_move_made(m)
                                		}

                                		if(ev.id=="promking")
                                		{
                                			m.prom_piece=fromFenChar('k')
                                			Builder.closeStage(dialog)
                                			manual_move_made(m)
                                		}
                                	}

                                	do_draw=false

                                	put_piece_xy("board",'?',translit_light,
                                		piece_cx(fileOf(drag_to))+piece_size/5,piece_cy(rankOf(drag_to))-piece_size/10,
                             			PIECE_COLORS(b.turn),new Font(piece_size),force_text="?")

                                    Builder.MyStage(dialog,set_handler=handler,unclosable=true,title="Promotion")

                                }
                            }
                            
                        }
                        
                        layers("drag").gc.clearRect(0, 0, canvas_size, canvas_size)
                    
                    }
                    else
                    {
                        if(setup)
                        {
                            mouse_released=true
                            
                            b.rep(drag_from)=NO_PIECE
                        }
                        else
                        {
                            println("out of board")
                        }
                    }
                    
                    if(do_draw) draw_board
                    
                }
            }
            
            if(etype=="MOUSE_DRAGGED")
            {
                
                if(!is_drag_going)
                {
                    is_drag_going=true
                    
                    drag_from=fromFileRank(px_to_index(x),px_to_index(y))

                    drag_piece=b.rep(drag_from)
                    
                    b.rep(drag_from)=NO_PIECE

                    draw_board
                    
                    drag_dx=piece_cx(fileOf(drag_from))-x
                    drag_dy=piece_cy(rankOf(drag_from))-y
                    
                }
                else
                {
                    
                    layers("drag").gc.clearRect(0, 0, canvas_size, canvas_size)
         
                    put_piece_xy("drag",toBlack(drag_piece),translit_light,x+drag_dx,y+drag_dy,PIECE_COLORS(colorOf(drag_piece)))
                    
                }
                
            }
            
            if(etype=="MOUSE_CLICKED")
            {
                
                if(mouse_released)
                {
                    mouse_released=false
                }
                else if(setup)
                {
                    
                    val clicked_sq=fromFileRank(px_to_index(x),px_to_index(y))
                    
                    if((b.rep(clicked_sq)==setup_piece)||(mouseEvent.getButton()==MouseButton.SECONDARY))
                    {
                        b.rep(clicked_sq)=NO_PIECE
                    }
                    else
                    {
                        b.rep(clicked_sq)=setup_piece
                    }
                    
                    draw_board
                }

            }


		}
	}

	val board_canvas_handler=new myMouseEventHandler

	root.setOnMouseDragged(board_canvas_handler)
    root.setOnMouseClicked(board_canvas_handler)
    root.setOnMouseReleased(board_canvas_handler)

	def put_piece_xy(l:String,p:TPiece,m:Map[Char,Char],x:Int,y:Int,c:Color,set_font:Font=chess_font,force_text:String="")
	{
		layers(l).gc.setFont(set_font)
		layers(l).gc.setFill(c)
		val text=if(force_text!="") force_text else ""+m(toFenChar(p))
		layers(l).gc.fillText(text,x,y)
	}

	def piece_cx(f:TFile):Int=true_index(f)*piece_size+piece_padding+margin

	def piece_cy(r:TRank):Int=true_index(r)*piece_size+piece_size-piece_padding+margin

	def set_from_fen(fen:String)
	{
		b.set_from_fen(fen)
	}

	var PIECE_COLORS=Map(WHITE->Color.rgb(255,255,255),BLACK->Color.rgb(0,0,0))
	var SQUARE_COLORS=Map(LIGHT->Color.rgb(127,127,127),DARK->Color.rgb(63,63,63))

	val color_mappings=Map(
		"whitepiececolor"->WHITE,
		"blackpiececolor"->BLACK,
		"lightsquarecolor"->LIGHT,
		"darksquarecolor"->DARK
		)

	for((k,v)<-color_mappings)
	{
		if(Builder.getsval(k)!=null)
		{
			if(k.contains("piece"))
			{
				PIECE_COLORS+=(v->GuiUtils.hexToColor(Builder.gss(k)))
			}			
			else
			{
				SQUARE_COLORS+=(v->GuiUtils.hexToColor(Builder.gss(k)))
			}
		}
	}

	var HIGHLIGHT_COLOR=Color.rgb(255,255,0)
	var ENGINE_HIGHLIGHT_COLOR=Color.rgb(127,127,255)
	var ENGINE_SCORE_POS_COLOR=Color.rgb(0,255,0)
	var ENGINE_SCORE_NEG_COLOR=Color.rgb(255,0,0)

	def highlight_square(sq:TSquare,layer:String="highlight",col:Color=HIGHLIGHT_COLOR)
	{
		val f=true_index(fileOf(sq))
		val r=true_index(rankOf(sq))

		layers(layer).gc.setFill(col)
		layers(layer).gc.fillRoundRect(
			f*piece_size+piece_padding+margin,
			r*piece_size+piece_padding+margin,
			piece_size-2*piece_padding,
			piece_size-2*piece_padding,
			piece_size/2,
			piece_size/2
			)
	}

	def highlight_move(m:move)
	{
		highlight_square(m.from)
		highlight_square(m.to)
	}

	def clear_engine
	{
		layers("engine").gc.clearRect(0,0,canvas_size,canvas_size)
	}

	def highlight_engine_move(pv:String,score:Int=0)
	{

		clear_engine

		val parts=pv.split(" ")
		val algeb=parts(0)
		if(algeb.length< 4) return

		val algebparts=algeb.grouped(2).toArray

		val from=fromAlgeb(algebparts(0))
		val to=fromAlgeb(algebparts(1))

		var prom_piece=NO_PIECE

		if(algeb.length>4) prom_piece=fromFenChar(algeb(4))

		val m=move(from=from,to=to,prom_piece=prom_piece)

		highlight_square(m.from,"engine",ENGINE_HIGHLIGHT_COLOR)
		highlight_square(m.to,"engine",ENGINE_HIGHLIGHT_COLOR)

		val eagc=layers("enginearrow").gc

		eagc.setStroke(scoreColorOf(score))
		eagc.setFill(scoreColorOf(score))
		eagc.setLineWidth(6)

		val tox=true_index(fileOf(to))*piece_size+piece_size/2+margin
		val toy=true_index(rankOf(to))*piece_size+piece_size/2+margin

		eagc.strokeLine(
			true_index(fileOf(from))*piece_size+piece_size/2+margin,
			true_index(rankOf(from))*piece_size+piece_size/2+margin,
			tox,
			toy
			)

		eagc.fillOval(tox-9,toy-9,18,18)

	}

	def clear_score
	{
		layers("enginescore").gc.clearRect(0,0,canvas_size,canvas_size)
		layers("enginearrow").gc.clearRect(0,0,canvas_size,canvas_size)
	}

	def scoreColorOf(score:Int):Color=
	{
		if(score>=0) ENGINE_SCORE_POS_COLOR else ENGINE_SCORE_NEG_COLOR
	}

	def print_score(score:Int)
	{
		clear_score
		val esgc=layers("enginescore").gc
		esgc.setFill(scoreColorOf(score))
		esgc.setStroke(scoreColorOf(score))
		esgc.setLineWidth(4)
		esgc.setFont(new Font("Courier New",120))
		esgc.strokeText((if(score>0) "+" else "")+score,100,220)
	}

	def clear_highlight
	{
		layers("highlight").gc.clearRect(0,0,canvas_size,canvas_size)
	}

	def draw_board
	{

		layers("boardsq").gc.clearRect(0,0,canvas_size,canvas_size)
		layers("boardcoord").gc.clearRect(0,0,canvas_size,canvas_size)
		layers("board").gc.clearRect(0,0,canvas_size,canvas_size)

		if(showcoords) for(i<-0 to BOARD_SIZE-1)
		{
			layers("boardcoord").gc.setFont(new Font(margin/2))
			layers("boardcoord").gc.setFill(Color.rgb(0,0,0))

			val cx=true_index(i)*piece_size+margin+piece_size/2-margin/4
			val cy=true_index_inv(i)*piece_size+margin+piece_size/2+margin/4

			layers("boardcoord").gc.fillText(""+('a'+i).toChar,cx+margin/8,canvas_size-margin/3)
			layers("boardcoord").gc.fillText(""+('1'+i).toChar,margin/3,cy)
		}

		for(sq<-0 to BOARD_AREA-1)
		{
			val f=fileOf(sq)
			val r=rankOf(sq)

			val p=b.rep(sq)

			val ci=colorIndexOf(sq)

			val t=translits(ci)

			layers("boardsq").gc.setFill(SQUARE_COLORS(ci))
			layers("boardsq").gc.fillRect(f*piece_size+margin,r*piece_size+margin,piece_size,piece_size)

			put_piece_xy("board",toBlack(p),translit_light,piece_cx(f),piece_cy(r),PIECE_COLORS(colorOf(p)))
		}
	}

	b.reset

	val board_opacity=Utils.parse[Double](Builder.gss("boardopacity","0.2"),0.2)

	val layers=Map(
			"boardsq"->Layer(board_opacity),
			"board"->Layer(1.0),
			"boardcoord"->Layer(1.0),
			"highlight"->Layer(0.35),
			"engine"->Layer(0.35),
			"enginearrow"->Layer(1.0),
			"enginescore"->Layer(1.0),
			"drag"->Layer(1.0)
		)

	val layers_list=List("boardsq","board","boardcoord","highlight","engine","enginearrow","enginescore","drag")

	for(name<-layers_list) root.getChildren.add(layers(name).canvas)

	draw_board

}

class setupboard(
	val piece_size:Int=45,
	val apply_callback:(String)=>Unit
	)
{

	val gb=new guiboard(piece_size)

	gb.setup=true

	gb.b.reset

	gb.draw_board

	val root=new HBox(5)

	val bvbox=new VBox(5)

	bvbox.getChildren().add(gb.rooth)

	root.getChildren().add(bvbox)

	val phboxw=new HBox(8)
	phboxw.setPadding(new Insets(5,5,5,5))
	val phboxb=new HBox(8)
	phboxb.setPadding(new Insets(5,5,5,5))

	def change_piece(p:Char)()
	{
		gb.setup_piece=fromFenChar(p)
	}

	for(c<-List('K','Q','R','B','N','P'))
	{
		val b=new MyButton(""+gb.translit_light(c),change_piece(c))
		b.setFont(gb.small_chess_font)
		phboxw.getChildren().add(b)
	}

	for(c<-List('k','q','r','b','n','p'))
	{
		val b=new MyButton(""+gb.translit_light(c),change_piece(c))
		b.setFont(gb.small_chess_font)
		phboxb.getChildren().add(b)
	}

	bvbox.getChildren().addAll(phboxw,phboxb)

	val controls=new VBox(5)
	controls.setPadding(new Insets(5,5,5,5))

	root.getChildren().add(controls)

	val castling_combo=new MyCombo(
		List("-","KQkq","KQk","KQq","KQ","Kkq","Kk","Kq","K","Qkq","Qk","Qq","Q","kq","k","q"),"setupcastling",(String)=>{})

	val turn_combo=new MyCombo(
		List("White","Black"),"setupturn",(String)=>{})

	val ep_squares="-"+:(for(f<-0 to 7 ; r<- (if(settings.variant=="Horde") List(6,5,2,1) else List(5,2)))
		yield toAlgeb(fromFileRank(f,r))).toList

	val ep_square_combo=new MyCombo(
		ep_squares,"setupepsquare",(String)=>{})

	val fullmove_number_text=new TextField()
	fullmove_number_text.setText(""+settings.setup_fullmove_number)

	val halfmove_clock_text=new TextField()
	halfmove_clock_text.setText(""+settings.setup_halfmove_clock)

	def update
	{
		val castling_rights_str=gb.b.castlingRightsAsString
		settings.setup_castling=castling_rights_str
		castling_combo.select(castling_rights_str)

		val ep_square_algeb=toAlgeb(gb.b.ep_square)
		settings.setup_ep_square=ep_square_algeb
		ep_square_combo.select(ep_square_algeb)

		val turn_str=if(gb.b.turn==WHITE) "White" else "Black"
		settings.setup_turn=turn_str
		turn_combo.select(turn_str)

		val fullmove_number=gb.b.fullmove_number
		settings.setup_fullmove_number=fullmove_number
		fullmove_number_text.setText(""+fullmove_number)

		val halfmove_clock=gb.b.halfmove_clock
		settings.setup_halfmove_clock=halfmove_clock
		halfmove_clock_text.setText(""+halfmove_clock)
	}

	def apply()
	{
		settings.set("setupfullmovenumber",fullmove_number_text.getText())
		settings.set("setuphalfmoveclock",halfmove_clock_text.getText())
		gb.b.setCastlingRightsFromFen(settings.setup_castling)
		gb.b.ep_square=fromAlgeb(settings.setup_ep_square)
		gb.b.setCastlingRightsFromFen(settings.setup_castling)
		gb.b.turn=(if(settings.setup_turn=="White") WHITE else BLACK)
		gb.b.fullmove_number=settings.setup_fullmove_number
		gb.b.halfmove_clock=settings.setup_halfmove_clock
		apply_callback(gb.b.report_fen)
	}

	def clear()
	{
		gb.b.clear
		gb.draw_board
	}

	def revert()
	{
		gb.b.revert
		gb.draw_board
	}

	val clear_button=new MyButton("Clear board",clear)
	val revert_button=new MyButton("Revert colors",revert)

	val apply_button=new MyButton("Apply changes",apply,ButtonStyles.round)

	controls.getChildren().addAll(
		new Label("Turn:"),
		turn_combo.n,
		new Label("Castling rights:"),
		castling_combo.n,
		new Label("Ep square:"),
		ep_square_combo.n,
		new Label("Fullmove number:"),
		fullmove_number_text,
		new Label("Halfmove clock:"),
		halfmove_clock_text,
		clear_button,
		revert_button,
		apply_button
		)

	update

	def set_from_fen(fen:String)
	{
		gb.b.set_from_fen(fen)
		update
		gb.draw_board
	}

}