package gui2;

public class Square implements java.io.Serializable
{
    
    public Boolean valid=false;
    
    // file 0 .. 7 ( a = 0, h = 7 )
    public int i;
    // rank 0 .. 7 ( 8 = 0, 1 = 7 )
    public int j;
    
    public void print()
    {
        System.out.println(valid?
                "[ Square : i = "+i+
                " , j = "+j+
                " , algeb = "+to_algeb()+" ]"
                :
                "[ Square : invalid ]"
        );
    }
    
    public String to_algeb()
    {
        
        if(!valid)
        {
            return null;
        }
        
        char file=(char)i;
        
        file+='a';
        
        char rank=(char)(7-j);
        
        rank+='1';
        
        String algeb=""+file+rank;
        
        return algeb;
        
    }
    
    public Boolean from_ij(int set_i,int set_j)
    {
        if((set_i<0)||(set_i>7)||(set_j<0)||(set_j>7))
        {
            return (valid=false);
        }
        
        i=set_i;
        j=set_j;
        
        return (valid=true);
    }
    
    public Boolean from_algeb(String algeb)
    {
        
        if(algeb.length()<2)
        {
            return (valid=false);
        }
        
        char file=algeb.charAt(0);
        
        if((file<'a')||(file>'h'))
        {
            return (valid=false);
        }
                
        char rank=algeb.charAt(1);
        
        if((rank<'1')||(rank>'8'))
        {
            return (valid=false);
        }
        
        i=file-'a';
        
        j=7-(rank-'1');
        
        return (valid=true);
        
    }
    
    public Boolean is_equal(Square s)
    {
        return ((s.i==i)&&(s.j==j));
    }
    
    public Square()
    {
        
    }
    
    public Square(String algeb)
    {
        from_algeb(algeb);
    }
    
    public Square(int set_i,int set_j)
    {
        from_ij(set_i,set_j);
    }
    
}
