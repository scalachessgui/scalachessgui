package gui2;

public class MyTokenizer
{
    
    String content;
    
    public String get_token_upto(String sep)
    {
        
        String token=null;
        
        if(content==null)
        {
            return null;
        }
        
        int index=content.indexOf(sep);
        
        if(index<0)
        {
            if(content.length()>0)
            {
                token=content;
            }
            content="";
            return token;
        }
        
        token=content.substring(0,index);
        content=content.substring(index+sep.length());
        return token;
    }
    
    public String start_with(String start)
    {
        String old_content=content;
        String token=get_token();
        
        if(token==null)
        {
            content=old_content;
            return null;
        }
        
        if(token.length()<start.length())
        {
            content=old_content;
            return null;
        }
        
        if(token.substring(0,start.length()).equals(start))
        {
            
            if(token.length()==start.length())
            {
                return "";
            }
            
            return token.substring(start.length());
            
        }
        
        content=old_content;
        return null;
    }
    
    public String get_char()
    {
        
        if(content.length()==0)
        {
            return null;
        }
        
        String c=""+content.charAt(0);
        content=content.substring(1);
        return c;
        
    }
    
    public Boolean is_whitespace(String c)
    {
        return (c.equals(" "))||(c.equals("\t"));
    }
    
    public void flush()
    {
        
        String c;
        
        do
        {
            c=get_char();
            if(c==null)
            {
                return;
            }
        }while(is_whitespace(c));
        
        content=c+content;
        
    }
    
    public String get_rest()
    {
        return content;
    }
    
    public int get_int()
    {
        
        String token=get_token();
        
        if(token!=null)
        {
            return Integer.parseInt(token);
        }
        
        return 0;
        
    }
    
    public String get_token()
    {
        
        String token=null;
        
        flush();
        
        String c;
        
        while((c=get_char())!=null)
        {
            if(is_whitespace(c))
            {
                return token;
            }
            if(token==null)
            {
                token=c;
            }
            else
            {
                token+=c;
            }
        }
        
        return token;
        
    }
    
    public MyTokenizer(String what)
    {
        
        content=what;
        
    }
    
}
