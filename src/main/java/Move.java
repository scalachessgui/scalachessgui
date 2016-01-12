package gui2;

public class Move extends RawMove implements java.io.Serializable
{
    
    public Square ep_square=new Square();
    
    public Piece prom_piece=new Piece();
    
    public void print()
    {
        if(valid)
        {
            System.out.println(
                    "[ Move "+super.print_as_string()+
                    " : ep square = "+ep_square.to_algeb()+
                    " , prom piece = ("+prom_piece.fen_char+") ]"
            );
            return;
        }
        
        System.out.println("[ Move : invalid ]");
    }
    
    public String to_algeb()
    {
        if(!valid)
        {
            return null;
        }
        
        String algeb=super.to_algeb();
        
        if(prom_piece.is_prom_piece())
        {
            algeb+=prom_piece.lower_fen_char();
        }
        
        return algeb;
    }
    
    public Boolean from_algeb(String algeb)
    {
        
        prom_piece.clear();
        
        if(algeb.length()>5)
        {
            return (valid=false);
        }
        
        if(algeb.length()<4)
        {
            return (valid=false);
        }
        
        if(!super.from_algeb(algeb.substring(0, 4)))
        {
            return (valid=false);
        }
        
        if(algeb.length()>4)
        {
            prom_piece.from_fen_char(algeb.charAt(4));
            if(!prom_piece.is_prom_piece())
            {
                return (valid=false);
            }
        }
        
        return (valid=true);
    }
    
    public Boolean from_squares_and_prom_piece(Square set_from,Square set_to,Piece set_prom_piece)
    {
        if(!super.from_squares(set_from, set_to))
        {
            return (valid=false);
        }
        
        prom_piece.from_fen_char_safe(set_prom_piece.fen_char);
        
        return (valid=true);
    
    }
    
    public Boolean is_equal(Move m)
    {
        return (super.is_equal(m)&&(prom_piece.type()==m.prom_piece.type()));
    }
    
    public Move()
    {
        super();
    }
    
    public Move(String algeb)
    {
        from_algeb(algeb);
    }
    
    public Move(Square set_from,Square set_to)
    {
        super(set_from,set_to);
    }
    
    public Move(Square set_from,Square set_to,Piece set_prom_piece)
    {
        from_squares_and_prom_piece(set_from,set_to,set_prom_piece);
    }
    
}
