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

// engine adapter

public class Engine extends EngineLine
{
    
    private int atom_i=0;
    
    private static Clipboard clip=Clipboard.getSystemClipboard();
    
    public Boolean is_atomkraft()
    {
        return(uci_engine_path.contains("atomkraft"));
    }
    
    public int multipv=1;
    
    public void set_multipv(int set_multipv)
    {
        if(!is_engine_installed())
        {
            return;
        }
        issue_command(is_atomkraft()?"3\n"+set_multipv+"\n":"setoption name MultiPV value "+set_multipv+"\n");
        multipv=set_multipv;
    }
    
    final static int MAX_LINES=500;
    public EngineLine lines[]=new EngineLine[MAX_LINES];
    
    static FileChooser file_chooser=new FileChooser();
    
    static Stage file_chooser_stage=new Stage();
    
    public String uci_engine_path;
    
    
    private Runnable runnable_engine_read_thread;
        
    private Thread engine_read_thread;
        
    private ProcessBuilder uci_engine_process_builder;
    private Process uci_engine_process;
    
    private InputStream engine_in;
    private OutputStream engine_out;
    
    public Boolean engine_intro=true;
    
    public String uci_puff="";
    
    public Boolean engine_running=false;

    String bestmove_algeb="";
    Move bestmove=new Move();
    
    public void receive_intro(String what)
    {
        //System.out.println("engine intro: "+what);
    }
    
    public void update_engine()
    {
        System.out.println("depth: "+depth+" score: "+score_numerical+" pv: "+pv);
    }
    
    public void update_engine_forced()
    {
        update_engine();
    }
    
    public String get_config_path()
    {
        return null;
    }
    
    public void set_config_path(String set_path)
    {
        
    }
    
    public void consume_engine_out(String uci)
    {
        
        if(engine_intro)
        {
            
            uci=uci.replaceAll("^info string ", "");
            
            uci_puff+=uci+"\n";
            
            Platform.runLater(new Runnable()
            {
                
                public void run()
                {
                    
                    //Gui.engine_text_area.setText(uci_puff);
                    receive_intro(uci_puff);
                    
                }
               
            });
            
            return;
            
        }
        
        uci=uci.replaceAll("[\\r\\n]*","");
        uci+=" ";
        
        //System.out.println(uci);
        
        Pattern get_bestmove = Pattern.compile("(^bestmove )(.*)");
        Matcher bestmove_matcher = get_bestmove.matcher(uci);
        
        if (bestmove_matcher.find( )) {
           engine_running=false;
           update_engine_forced();
           return;
        }
        
        if(!uci.matches("^info .*"))
        {
            return;
        }
        
        int curr_multipv=extract_int(uci,"multipv",1)-1;
        
        if(is_atomkraft())
        {
            curr_multipv=atom_i;
            atom_i++;
            if(atom_i>=multipv)
            {
                atom_i=0;
            }
        }
        
        if(curr_multipv==0)
        {
            
            extract_line(fen,uci);

            String[] pv_parts=pv.split(" ");

            bestmove_algeb=pv_parts[0];
            
        }
        
        lines[curr_multipv].extract_line(fen,uci);
        
        //Gui.update_engine();
        update_engine();
        
    }
    
    public void issue_command(String command)
    {
        try
        {
                        
            engine_out.write(command.getBytes());
            engine_out.flush();

        }
        catch(IOException ex)
        {

        }
    }
    
    public String fen="";
    public void go()
    {
        go_inner(-1);
    }
    public void go_inner(int depth)
    {
        
        if(!is_engine_installed())
        {
            return;
        }
        
        if(engine_running)
        {
            
        }
        else
        {
            
            engine_intro=false;
            String set_fen_command="";
            if(!fen.equals(""))
            {
                set_fen_command="position fen "+fen+"\n";
            }
            String command=set_fen_command+
                    (depth<0?"go infinite\n":"go depth "+depth+"\n");
            
            if(is_atomkraft())
            {
                ClipboardContent content = new ClipboardContent();
                content.putString(fen);
                clip.setContent(content);
                
                //System.out.println("atomkraft set fen "+fen);
                
                command="1\n4\n";
            }
            
            atom_i=0;
            
            //System.out.println(command);
            issue_command(command);
            engine_running=true;
            
        }
    }
    
    public void stop()
    {
        
        if(engine_running)
        {
            
            issue_command(is_atomkraft()?"s\n":"stop\n");

            while(engine_running)
            {
                
                try
                {
                    
                    Thread.sleep(100);
                    
                }
                catch(InterruptedException ex)
                {
                    
                    Thread.currentThread().interrupt();
                    
                }
            
            }
            
            
        }
        
    }
    
    public Boolean is_engine_installed()
    {
        if(uci_engine_path==null)
        {
            return false;
        }
        if(uci_engine_path.equals(""))
        {
            return false;
        }
        return true;
    }
    
    public void stop_engine_process()
    {
        
        if(is_engine_installed())
        {
            engine_read_thread.interrupt();
    
            uci_engine_process.destroy();
            
            uci_engine_path="";
        }
        
    }
    
    
    public Boolean load_engine(String set_path)
    {

        //System.out.println("loading engine from path "+set_path);
        
        stop_engine_process();
        
        engine_intro=true;
        uci_puff="";
        
        if(set_path==null)
        {
            uci_engine_path="";
        }
        else
        {
            uci_engine_path=set_path;
        }
        
        if(!(uci_engine_path.equals("")))
        {

            uci_engine_process_builder=new ProcessBuilder(uci_engine_path);

            try {
                   uci_engine_process=uci_engine_process_builder.start();
                }
            catch(IOException ex)
                {
                    System.out.println("error loading engine");
                    uci_engine_path="";
                    
                    return false;
                }
            
            engine_in=uci_engine_process.getInputStream();
            engine_out=uci_engine_process.getOutputStream();

            runnable_engine_read_thread=new Runnable(){
                public void run()
                {
                    
                    String buffer="";
                    
                    while (!Thread.currentThread().isInterrupted())
                    {
                   
                        try
                        {

                             char chunk=(char)engine_in.read();
                             
                             //System.out.print(chunk);

                             if(chunk=='\n')
                             {
                                 //consume_engine_out(buffer);
                                 
                                 consume_engine_out(buffer);
                                 
                                 buffer="";
                             }
                             else
                             {
                                 buffer+=chunk;
                             }

                        }
                        catch(IOException ex)
                        {

                            System.out.println("engine read IO exception");

                        }

                    }
                    
                }
                    
            };
            
            engine_read_thread=new Thread(runnable_engine_read_thread);

            engine_read_thread.start();
            
            set_config_path(uci_engine_path);
            
            System.out.println("engine loaded "+uci_engine_path);
            
            return true;

        }
        
        return false;
        
    }
    
    
    public Engine()
    {
        super();
        
        for(int i=0;i<MAX_LINES;i++)
        {
            lines[i]=new EngineLine();
        }
    }
    
}
