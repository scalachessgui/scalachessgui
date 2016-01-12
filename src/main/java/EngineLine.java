package gui2;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import javafx.application.Platform;
import javafx.scene.input.Clipboard;
import javafx.scene.input.ClipboardContent;
import javafx.stage.FileChooser;
import javafx.stage.Stage;

public class EngineLine
{
    
    public String pv="";
    public String pvs="";
    public int depth=0;
    public int time=0;
    public int score_mate=0;
    public int score_cp=0;
    public String score_verbal="";
    public int score_numerical=0;
    public int nodes=0;
    public int nps=0;
    
    public String pv_to_san(String fen,String pv)
    {
	
        String[] algebs=pv.split(" ");
        
        Board dummy=new Board();
        
        dummy.set_from_fen(fen);
        
        String pv_san="";
        
        for(int i=0;i<algebs.length;i++)
        {
            String algeb=algebs[i];
            
            if((algeb.length()>=4)&&(algeb.length()<=5))
            {
                Move m=new Move(algeb);
                
                String san=dummy.to_san(m);
                
                pv_san+=san+" ";
                
                dummy.make_move(m);
            }
        }
        
        return pv_san;
    }
    
    public void extract_line(String fen,String uci)
    {
        pv=extract_pv(uci);
        
        pvs=pv_to_san(fen,pv);
        
        depth=extract_int(uci,"depth",depth);
        
        time=extract_int(uci,"time",time);
        
        score_cp=extract_int(uci,"score cp",score_cp);
        
        nodes=extract_int(uci,"nodes",nodes);
        
        nps=extract_int(uci,"nps",nps);
        
        if(uci.contains("score"))
        {
            score_mate=extract_int(uci,"score mate",10000);

            if(score_mate!=10000)
            {
               score_verbal="mate "+score_mate;
               score_numerical=
                       score_mate<0?
                       -10000-score_mate
                       :
                       10000-score_mate
                       ;
            }
            else
            {
               score_verbal="cp "+score_cp;
               score_numerical=score_cp;
            }
        }
    }
    
    public int extract_int(String uci,String field,int default_value)
    {
    
        Pattern get_int = Pattern.compile(" "+field+" (-?[0-9]+)");
        Matcher int_matcher = get_int.matcher(uci);
        
        if (int_matcher.find( )) {
           return(Integer.parseInt(int_matcher.group(1)));
        }
        
        return default_value;
    }
    
    public String extract_string_inner(String uci,String field,String default_value,Boolean all)
    {
    
        Pattern get_string = Pattern.compile(" "+field+(all?" (.*)":" ([^ ]+)"));
        Matcher string_matcher = get_string.matcher(uci);
        
        if (string_matcher.find( )) {
           return(string_matcher.group(1));
        }
        
        return default_value;
    }
    
    public String extract_string(String uci,String field,String default_value)
    {
        return extract_string_inner(uci,field,default_value,false);
    }
    
    public String extract_pv(String uci)
    {
        return extract_string_inner(uci,"pv",pv,true);
    }
    
    public EngineLine()
    {
        
    }
    
}
