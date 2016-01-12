package settings

import scala.xml._
import java.io._

import game._
import square.square._
import builder._
import data._
import gui2.Encode32

object settings
{

	val SUPPORTED_VARIANTS=List(
			"Standard",
			"Atomic",
			"King of the Hill",
			"Chess960",
			"Antichess",
			"Horde",
			"From Position",
			"Three-check"
		)

	val ANNOTATIONS=List("!!","!","!?","-","?!","?","??")

	val ANNOT_COLORS=Map(
			"!!"->"#00ff00",
			"!"->"#007f00",
			"!?"->"#0000ff",
			"-"->"#0000af",
			"?!"->"#00007f",
			"?"->"#7f0000",
			"??"->"#ff0000"
		)

	val DEFAULT_BUILD_CUTOFF=10
	val DEFAULT_UPDATE_RESULT=false
	val DEFAULT_INC_MOVE_COUNT=false
	val DEFAULT_ADD_GAMES=true
	val DEFAULT_FLIP=false

	val DEFAULT_BUILD_DIR="sorted"
	val DEFAULT_ENGINE_DIR="C:"+File.separator+"Engine"
	val DEFAULT_PGN_DIR="sorted"
	val DEFAULT_ENGINE_PATH=""

	val DEFAULT_SETUP_CASTLING="-"
	val DEFAULT_SETUP_EP_SQUARE="-"
	val DEFAULT_SETUP_TURN="White"
	val DEFAULT_SETUP_FULLMOVE_NUMBER=1
	val DEFAULT_SETUP_HALFMOVE_CLOCK=0

	////////////////////////////////////////////////////

	var variant="Standard"

	var game_pgn=""

	var game_current_line=""

	var build_cutoff=DEFAULT_BUILD_CUTOFF

	var update_result=DEFAULT_UPDATE_RESULT
	var inc_move_count=DEFAULT_INC_MOVE_COUNT
	var add_games=DEFAULT_ADD_GAMES
	var flip=DEFAULT_FLIP

	var build_dir=DEFAULT_BUILD_DIR
	var engine_dir=DEFAULT_ENGINE_DIR
	var pgn_dir=DEFAULT_PGN_DIR

	var setup_castling=DEFAULT_SETUP_CASTLING
	var setup_ep_square=DEFAULT_SETUP_EP_SQUARE
	var setup_turn=DEFAULT_SETUP_TURN
	var setup_fullmove_number=DEFAULT_SETUP_FULLMOVE_NUMBER
	var setup_halfmove_clock=DEFAULT_SETUP_HALFMOVE_CLOCK

	////////////////////////////////////////////////////

	def get_annot_color(annot:String):String=
	{
		if(ANNOT_COLORS.contains(annot)) return ANNOT_COLORS(annot)
		"#7f7f7f"
	}

	def get(key:String):String=
	{
		if(key=="variant") return variant
		if(key=="currentbook") return get_current_book()
		if(key=="buildcutoff") return ""+build_cutoff
		if(key=="setupcastling") return setup_castling
		if(key=="setupepsquare") return setup_ep_square
		if(key=="setupturn") return setup_turn
		if(key=="setupfullmovenumber") return ""+setup_fullmove_number
		if(key=="setuphalfmoveclock") return ""+setup_halfmove_clock
		if(key=="builddir") return build_dir
		if(key=="enginedir") return engine_dir
		if(key=="pgndir") return pgn_dir
		""
	}

	def set(key:String,value:String)
	{
		if(key=="variant") variant=value
		if(key=="currentbook") set_current_book(value)
		if(key=="buildcutoff") build_cutoff=value.toInt
		if(key=="setupcastling") setup_castling=value
		if(key=="setupepsquare") setup_ep_square=value
		if(key=="setupturn") setup_turn=value
		if(key=="setupfullmovenumber") setup_fullmove_number=Utils.parse[Int](value,DEFAULT_SETUP_FULLMOVE_NUMBER)
		if(key=="setuphalfmoveclock") setup_halfmove_clock=Utils.parse[Int](value,DEFAULT_SETUP_HALFMOVE_CLOCK)
		if(key=="builddir") build_dir=value
		if(key=="enginedir") engine_dir=value
		if(key=="pgndir") pgn_dir=value
	}

	def set_variant(v:String):String=
	{
		if(!SUPPORTED_VARIANTS.contains(v))
		{
			return s"Error. Variant '$v' not supported."
		}

		variant=v

		return s"Ok. Variant set to '$v'."
	}

	def load
	{
		variant=Builder.gss("variant","Standard")

		build_cutoff=Builder.gsi("buildcutoff",DEFAULT_BUILD_CUTOFF)

		update_result=Builder.gsb("updateresult",DEFAULT_UPDATE_RESULT)
		inc_move_count=Builder.gsb("incmovecount",DEFAULT_INC_MOVE_COUNT)
		add_games=Builder.gsb("addgames",DEFAULT_ADD_GAMES)
		flip=Builder.gsb("flip",DEFAULT_FLIP)

		build_dir=Builder.gss("builddir",DEFAULT_BUILD_DIR)
		engine_dir=Builder.gss("enginedir",DEFAULT_ENGINE_DIR)
		pgn_dir=Builder.gss("pgndir",DEFAULT_PGN_DIR)

		game_pgn=Encode32.encode(Builder.gss("gamepgn",""),false)
		game_current_line=Encode32.encode(Builder.gss("gamecurrentline",""),false)
	}

	def getvariant=variant

	def get_current_book(v:String=getvariant):String=
	{
		val path=s"variantentries#$v#currentbook"

		val cb=Builder.gs(path,"default")

		Builder.setval(path,cb)

		cb
	}

	def set_current_book(cb:String,v:String=getvariant)
	{
		val path=s"variantentries#$v#currentbook"

		Builder.setval(path,cb)
	}

	def get_variant_engine_path(v:String=getvariant):String=
	{
		val path=s"variantentries#$v#enginepath"

		val ep=Builder.gs(path,DEFAULT_ENGINE_PATH)

		ep
	}

	def set_variant_engine_path(ep:String,v:String=getvariant)
	{
		val path=s"variantentries#$v#enginepath"

		Builder.setval(path,ep)
	}

	def save
	{
		Builder.setsval("variant",variant)
		Builder.setsval("buildcutoff",""+build_cutoff)
		Builder.setsval("incmovecount",""+inc_move_count)
		Builder.setsval("updateresult",""+update_result)
		Builder.setsval("addgames",""+add_games)
		Builder.setsval("flip",""+flip)
		Builder.setsval("builddir",build_dir)
		Builder.setsval("enginedir",engine_dir)
		Builder.setsval("pgndir",pgn_dir)
		Builder.setsval("gamepgn",Encode32.encode(game_pgn,true))
		Builder.setsval("gamecurrentline",Encode32.encode(game_current_line,true))
	}
}