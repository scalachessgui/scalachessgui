package gui2;

public class RawMove
{
    
    Boolean valid=false;
    
    public Square from=new Square();
    
    public Square to=new Square();
    
    public String print_as_string()
    {
        return(valid?
                "[ RawMove : from = "+from.to_algeb()+
                " , to = "+to.to_algeb()+
                " , algeb = "+to_algeb()+" ]"
                :
                "[ RawMove : invalid ]"
        );
    }
    
    public void print()
    {
        System.out.println(print_as_string());
    }
    
    public String to_algeb()
    {
        if(!valid)
        {
            return null;
        }
        
        String from_algeb=from.to_algeb();
        String to_algeb=to.to_algeb();
        
        String algeb=from_algeb+to_algeb;
        
        return algeb;
    }
    
    public Boolean from_algeb(String algeb)
    {
        
        if(algeb.length()!=4)
        {
            return (valid=false);
        }
        
        String from_algeb=algeb.substring(0, 2);
        
        String to_algeb=algeb.substring(2, 4);
        
        if(!from.from_algeb(from_algeb))
        {
            return (valid=false);
        }
        
        if(!to.from_algeb(to_algeb))
        {
            return (valid=false);
        }
        
        return (valid=true);
        
    }
    
    public Boolean from_squares(Square set_from,Square set_to)
    {
        if((!set_from.valid)||(!set_to.valid))
        {
            return (valid=false);
        }
        
        from=set_from;
        
        to=set_to;
        
        return (valid=true);
    }
    
    public Boolean is_equal(RawMove m)
    {
        return (from.is_equal(m.from)&&to.is_equal(m.to));
    }
    
    public RawMove()
    {
        
    }
    
    public RawMove(String algeb)
    {
        from_algeb(algeb);
    }
    
    public RawMove(Square set_from,Square set_to)
    {
        from_squares(set_from,set_to);
    }
    
}
