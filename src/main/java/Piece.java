package gui2;

public class Piece implements java.io.Serializable
{
    
    public final static int SLIDING=32;
    public final static int STRAIGHT=16;
    public final static int DIAGONAL=8;
    public final static int SINGLE=4;
    public final static int IS_PAWN=2;
    
    public final static int QUEEN=SLIDING|STRAIGHT|DIAGONAL;
    public final static int ROOK=SLIDING|STRAIGHT;
    public final static int BISHOP=SLIDING|DIAGONAL;
    
    public final static int KNIGHT=SINGLE;
    public final static int KING=SINGLE|STRAIGHT|DIAGONAL;
    public final static int PAWN=SINGLE|IS_PAWN;
    
    public final static int PIECE_NONE=0;
    
    public final static int COLOR_NONE=64;
    
    public final static int WHITE=1;
    public final static int BLACK=0;
    
    final static int TYPE=62;
    final static int COLOR=1;
    
    public char fen_char=' ';
    
    public static int color(char fen_char)
    {
        switch(fen_char)
        {
            case 'K':return WHITE;
            case 'k':return BLACK;
            case 'Q':return WHITE;
            case 'q':return BLACK;
            case 'R':return WHITE;
            case 'r':return BLACK;
            case 'B':return WHITE;
            case 'b':return BLACK;
            case 'N':return WHITE;
            case 'n':return BLACK;
            case 'P':return WHITE;
            case 'p':return BLACK;
            default: return COLOR_NONE;
        }
    }
    
    public static int type(char fen_char)
    {
        switch(fen_char)
        {
            case 'K':return KING;
            case 'k':return KING;
            case 'Q':return QUEEN;
            case 'q':return QUEEN;
            case 'R':return ROOK;
            case 'r':return ROOK;
            case 'B':return BISHOP;
            case 'b':return BISHOP;
            case 'N':return KNIGHT;
            case 'n':return KNIGHT;
            case 'P':return PAWN;
            case 'p':return PAWN;
            default: return PIECE_NONE;
        }
    }
    
    public static int code(char fen_char)
    {
        switch(fen_char)
        {
            case 'K':return KING|WHITE;
            case 'k':return KING|BLACK;
            case 'Q':return QUEEN|WHITE;
            case 'q':return QUEEN|BLACK;
            case 'R':return ROOK|WHITE;
            case 'r':return ROOK|BLACK;
            case 'B':return BISHOP|WHITE;
            case 'b':return BISHOP|BLACK;
            case 'N':return KNIGHT|WHITE;
            case 'n':return KNIGHT|BLACK;
            case 'P':return PAWN|WHITE;
            case 'p':return PAWN|BLACK;
            default: return PIECE_NONE;
        }
    }
    
    public int color()
    {
        return color(fen_char);
    }
    
    public int type()
    {
        return type(fen_char);
    }
    
    public int code()
    {
        return code(fen_char);
    }
    
    public char lower_fen_char()
    {
        if(type(fen_char)==PIECE_NONE)
        {
            return ' ';
        }
        return Character.toLowerCase(fen_char);
    }
    
    public char upper_fen_char()
    {
        if(type()==PIECE_NONE)
        {
            return ' ';
        }
        
        return Character.toUpperCase(fen_char);
    }
    
    public char fen_char_of_color(int color)
    {
        if(type()==PIECE_NONE)
        {
            return ' ';
        }
        
        if(color==WHITE)
        {
            return upper_fen_char();
        }
        else if(color==BLACK)
        {
            return lower_fen_char();
        }
        else
        {
            return ' ';
        }
    }
    
    public int inverse_color()
    {
        int color=color();
        
        if(color==COLOR_NONE)
        {
            return COLOR_NONE;
        }
        
        return color==WHITE?BLACK:WHITE;
    }
    
    public char inverse_fen_char()
    {
        
        return fen_char_of_color(inverse_color());
        
    }
    
    public Piece inverse_piece()
    {
        return new Piece(inverse_fen_char());
    }
    
    public Boolean is_prom_piece()
    {
        int type=type(fen_char);
        
        if((type==QUEEN)||(type==ROOK)||(type==BISHOP)||(type==KNIGHT))
        {
            return true;
        }
        
        return false;
    }

    public void from_fen_char(char set_fen_char)
    {
        fen_char=set_fen_char;
    }
    
    public void from_fen_char_safe(char set_fen_char)
    {
        fen_char=set_fen_char;
        if(type()==PIECE_NONE)
        {
            fen_char=' ';
        }
    }
    
    public Boolean empty()
    {
        if(fen_char==' ')
        {
            return true;
        }
        return (type()==PIECE_NONE);
    }
    
    public Boolean sliding()
    {
        return ((code()&SLIDING)!=0);
    }
    
    public void clear()
    {
        fen_char=' ';
    }
    
    public void copy(Piece p)
    {
        from_fen_char_safe(p.fen_char);
    }
    
    public Piece clone()
    {
        Piece clone=new Piece();
        clone.copy(this);
        return clone;
    }
    
    public Boolean is_equal(Piece p)
    {
        if(type()!=p.type())
        {
            return false;
        }
        if(color()!=p.color())
        {
            return false;
        }
        return true;
    }
    
    public Piece(char set_fen_char)
    {
        from_fen_char(set_fen_char);
    }
    
    public Piece(Piece set_piece)
    {
        from_fen_char_safe(set_piece.fen_char);
    }
    
    public Piece()
    {
        
    }
    
}
