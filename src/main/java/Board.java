package gui2;



import java.util.regex.Matcher;
import java.util.regex.Pattern;

class MoveDescriptor {
    
    public int to_i;
    public int to_j;
    
    public Boolean end_piece;
    public Boolean castling;
    public Boolean promotion;
    char prom_piece;
    
    public int next_vector;
    
    public MoveDescriptor()
    {
        end_piece=false;
        castling=false;
        promotion=false;
        prom_piece=' ';
    }
}

public class Board implements java.io.Serializable
{
    
    public final static int VARIANT_STANDARD=0;
    public final static int VARIANT_ATOMIC=1;
    
    public final static String[] variants={"Standard","Atomic"};
    
    public static int variant=VARIANT_STANDARD;
    
    public static void set_variant(String set_variant)
    {
        variant=get_variant_by_name(set_variant);
    }
    
    public static int get_variant_by_name(String variant)
    {
        for(int i=0;i<variants.length;i++)
        {
            if(variants[i].equals(variant))
            {
                return i;
            }
        }
        return 0;
    }
    
    public String report_empty_map(Boolean flip)
    {
        String empty_map="";
        for(int j=0;j<8;j++)
        {
            for(int i=0;i<8;i++)
            {
                if(board[flip?7-i:i][flip?7-j:j].empty())
                {
                    empty_map+="0";
                }
                else
                {
                    empty_map+="1";
                }
            }
            if(j<7)
            {
                empty_map+="\n";
            }
        }
        return empty_map;
    }
    
    public static String fen_to_raw(String fen)
    {
        String raw_fen=fen.replaceAll(" [^ ]+ [^ ]+$", "");
        
        return raw_fen;
    }
    
    final static int all_pieces[]={Piece.KING,Piece.QUEEN,Piece.ROOK,Piece.BISHOP,Piece.KNIGHT,Piece.PAWN};
    
    private int turn_of(Piece p)
    {
        return p.color()==Piece.WHITE?TURN_WHITE:TURN_BLACK;
    }
    
    private int curr_i=0;
    private int curr_j=0;
    
    private Piece current_move_gen_piece;
    int move_gen_curr_ptr;
    private void next_square()
    {
        Boolean stop=false;
        do
        {
            curr_i++;
            if(curr_i>7)
            {
                curr_i=0;
                curr_j++;
            }
            if(curr_j>7)
            {
                stop=true;
            }
            else
            {
                Piece gen_piece=board[curr_i][curr_j];
                stop=(
                        (!gen_piece.empty())
                        &&
                        (turn_of(gen_piece)==turn)
                );
            }
        }
        while(!stop);
        
        if(curr_j<8)
        {
            current_move_gen_piece=board[curr_i][curr_j];
            
            move_gen_curr_ptr=move_table_ptr[curr_i][curr_j][current_move_gen_piece.code()];
        }
        
    }
    
    private void init_move_generator()
    {
        curr_i=-1;
        curr_j=0;
        next_square();
    }
    
    private Boolean is_square_in_check(Square sq,int color)
    {
        
        Boolean check=is_square_in_check_by(sq,all_pieces,color);
        
        if(variant!=VARIANT_ATOMIC)
        {
            return check;
        }
        
        Boolean adjacent=is_square_in_check_by(sq,new int[]{Piece.KING},color);
        
        return check&&(!adjacent);
        
    }
    
    private Boolean is_square_in_check_by(Square sq,int all_pieces[],int color)
    {
        
        int attacker_color=color==Piece.WHITE?Piece.BLACK:Piece.WHITE;
        
        Boolean is_check=false;
        
        for(int p=0;p<all_pieces.length;p++)
        {
            
            int piece_code=all_pieces[p];
            int piece_type=piece_code&Piece.TYPE;
            int check_ptr=move_table_ptr[sq.i][sq.j][piece_code|color];
            
            int test_piece_code=piece_code|attacker_color;
            
            MoveDescriptor md;
            do
            {
                md=move_table[check_ptr];
                
                if(md.castling)
                {
                    check_ptr++;
                }
                else if(!md.end_piece)
                {
                    
                    int to_i=md.to_i;
                    int to_j=md.to_j;
                    
                    Piece to_piece=board[to_i][to_j].clone();
                    
                    if(piece_type==Piece.PAWN)
                    {
                        if(to_i==sq.i)
                        {
                            // pawn cannot check forward                            
                            to_piece.clear();
                        }
                    }
                    
                    if(to_piece.code()==test_piece_code)
                    {
                        is_check=true;
                    }
                    else
                    {
                        if(to_piece.empty())
                        {
                            check_ptr++;
                        }
                        else
                        {
                            check_ptr=md.next_vector;
                        }
                    }
                    
                }
                
            }while((!md.end_piece)&&(!is_check));
            
            
            if(is_check)
            {
                break;
            }
            
        }
        
        return is_check;
    }
    
    Move current_move;
    private Boolean next_pseudo_legal_move()
    {
        
        current_move=new Move();
        while(curr_j<8)
        {
            
            while(!move_table[move_gen_curr_ptr].end_piece)
            {
                
                MoveDescriptor md=move_table[move_gen_curr_ptr];
                
                int to_i=md.to_i;
                int to_j=md.to_j;
                
                Piece to_piece=board[to_i][to_j];
                
                int to_piece_code=to_piece.code();
                int to_piece_color=to_piece.color();
                
                current_move=new Move(new Square(curr_i,curr_j),new Square(to_i,to_j));
                
                current_move.prom_piece=new Piece(md.prom_piece);
                
                if(md.castling)
                {
                    
                    move_gen_curr_ptr++;
                    
                    if((curr_j==0)&&(to_i==6))
                    {
                        // black kingside
                        if(
                            (board[6][0].empty())
                            &&
                            (board[5][0].empty())
                            &&    
                            ((castling_rights&CASTLE_k)!=0)
                            &&
                            (!is_square_in_check(new Square(4,0),Piece.BLACK))
                            &&
                            (!is_square_in_check(new Square(5,0),Piece.BLACK))
                        )
                        {
                            return true;
                        }
                    }
                    
                    if((curr_j==0)&&(to_i==2))
                    {
                        // black queenside
                        if(
                            (board[3][0].empty())
                            &&
                            (board[2][0].empty())
                            &&
                            (board[1][0].empty())
                            &&    
                            ((castling_rights&CASTLE_q)!=0)    
                            &&
                            (!is_square_in_check(new Square(4,0),Piece.BLACK))
                            &&
                            (!is_square_in_check(new Square(3,0),Piece.BLACK))
                        )
                        {
                            return true;
                        }
                    }
                    
                    if((curr_j==7)&&(to_i==6))
                    {
                        // white kingside
                        if(
                            (board[6][7].empty())
                            &&
                            (board[5][7].empty())
                            &&    
                            ((castling_rights&CASTLE_K)!=0)
                            &&
                            (!is_square_in_check(new Square(4,7),Piece.WHITE))
                            &&
                            (!is_square_in_check(new Square(5,7),Piece.WHITE))
                        )
                        {
                            return true;
                        }
                    }
                    
                    if((curr_j==7)&&(to_i==2))
                    {
                        // white queenside
                        if(
                            (board[3][7].empty())
                            &&
                            (board[2][7].empty())
                            &&
                            (board[1][7].empty())
                            &&    
                            ((castling_rights&CASTLE_Q)!=0)    
                            &&
                            (!is_square_in_check(new Square(4,7),Piece.WHITE))
                            &&
                            (!is_square_in_check(new Square(3,7),Piece.WHITE))
                        )
                        {
                            return true;
                        }
                    }
                    
                }
                else if((!to_piece.empty())&&(to_piece_color==current_move_gen_piece.color()))
                {
                    
                    // own piece
                    if(current_move_gen_piece.sliding())
                    {
                        move_gen_curr_ptr=md.next_vector;
                    }
                    else
                    {
                        move_gen_curr_ptr++;
                    }
                }
                else
                {
                    
                    Boolean is_capture=!to_piece.empty();
                    
                    if(is_capture)
                    {
                    
                        // capture
                        if(current_move_gen_piece.sliding())
                        {
                            move_gen_curr_ptr=md.next_vector;
                        }
                        else
                        {
                            move_gen_curr_ptr++;
                        }
                        
                    }
                    else
                    {
                        move_gen_curr_ptr++;
                    }
                    
                    if(current_move_gen_piece.type()==Piece.PAWN)
                    {
                        
                        if(curr_i!=to_i)
                        {
                            // sidewise move may be ep capture
                            
                            Square test_sq=new Square(to_i, to_j);
                            
                            if(ep_square!=null)
                            {
                                
                                if(ep_square.is_equal(test_sq))
                                {
                                    is_capture=true;
                                }
                                
                            }
                        }
                        
                        if(is_capture)
                        {
                            // pawn captures only to the sides
                            if(curr_i!=to_i)
                            {
                                return true;
                            }
                        }
                        else
                        {
                            // pawn moves only straight ahead
                            if(curr_i==to_i)
                            {
                                if(Math.abs(to_j-curr_j)<2)
                                {
                                    // can always move one square forward
                                    return true;
                                }
                                else
                                {
                                    if(board[curr_i][curr_j+(to_j-curr_j)/2].empty())
                                    {
                                        // push by two requires empty passing square
                                        return true;
                                    }
                                }
                            }
                        }
                    }
                    else
                    {
                        return true;
                    }
                    
                }
                
            }
            
            next_square();
            
        }
        
        return false;
    }
    
    private Square where_is_king(int turn)
    {
        
        Boolean found=false;
        
        char search_king=turn==TURN_WHITE?'K':'k';
        
        int king_i=0;
        int king_j=0;
        
        for(int i=0;i<8;i++)
        {
            
            for(int j=0;j<8;j++)
            {
                
                if(board[i][j].fen_char==search_king)
                {
                    king_i=i;
                    king_j=j;
                    found=true;
                    break;
                }
                
                if(found){break;}
                
            }
            
        }
        
        if(!found)
        {
            return null;
        }
        
        return new Square(king_i,king_j);
        
    }
    
    private Boolean is_in_check(int turn)
    {
        Square king_sq=where_is_king(turn);
        
        if(king_sq!=null)
        {
            
            return is_square_in_check(king_sq,turn==TURN_WHITE?Piece.WHITE:Piece.BLACK);
            
        }
        
        return true;
    }
    
    private Boolean is_exploded(int turn)
    {
        return (where_is_king(turn)==null);
    }
    
    public Move san_to_move(String san)
    {
        
        Move m=new Move();
        
        m.from_algeb("a1a1");
        
        if(san.equals("O-O"))
        {
            if(turn==TURN_WHITE)
            {
                m.from_algeb("e1g1");
            }
            else
            {
                m.from_algeb("e8g8");
            }
            return m;
        }
        
        if(san.equals("O-O-O"))
        {
            if(turn==TURN_WHITE)
            {
                m.from_algeb("e1c1");
            }
            else
            {
                m.from_algeb("e8c8");
            }
            return m;
        }
        
        if(san.length()<2){return m;}
        
        Move dummy=new Move();
        
        char file_algeb=' ';
        char rank_algeb=' ';
        String target_algeb="";
        String algeb="";
        
        char piece=san.charAt(0);
        
        if((piece>='a')&&(piece<='z'))
        {
            // pawn move
            file_algeb=piece;
            piece='P';
            san=san.substring(1);
        }
        else
        {
            san=san.substring(1);
        }
        
        Boolean takes=false;
        
        if(san.charAt(0)=='x')
        {
            takes=true;
            san=san.substring(1);
        }
        
        if((piece=='P')&&(!takes))
        {
            rank_algeb=san.charAt(0);
            san=san.substring(1);
            target_algeb=""+file_algeb+rank_algeb;
            
            m.from_algeb("a1"+target_algeb);
            m.from.i=m.to.i;
            if(turn==TURN_WHITE)
            {
                if(board[m.to.i][m.to.j+1].fen_char=='P')
                {
                    m.from.j=m.to.j+1;
                }
                else if(board[m.to.i][m.to.j+2].fen_char=='P')
                {
                    m.from.j=m.to.j+2;
                }
            }
            else
            {
                if(board[m.to.i][m.to.j-1].fen_char=='p')
                {
                    m.from.j=m.to.j-1;
                }
                else if(board[m.to.i][m.to.j-2].fen_char=='p')
                {
                    m.from.j=m.to.j-2;
                }
            }
        }
        else if(piece=='P')
        {
            if(san.length()<2){return m;}
            target_algeb=""+san.charAt(0)+san.charAt(1);
            san=san.substring(2);
            m.from_algeb(file_algeb+"1"+target_algeb);
            if(turn==TURN_WHITE)
            {
                m.from.j=m.to.j+1;
            }
            else
            {
                m.from.j=m.to.j-1;
            }
        }
        else
        {
            
            // takes carries no information
            san=san.replace("x","");
                       
            Pattern get_algeb = Pattern.compile("([a-z0-9]*)");
            Matcher algeb_matcher = get_algeb.matcher(san);
            
            if(algeb_matcher.find())
            {
                algeb=algeb_matcher.group(0);
                
                if(algeb.length()==2)
                {
                    file_algeb=' ';
                    rank_algeb=' ';
                    
                    san=san.substring(2);
                    
                    m.from_algeb("a1"+algeb);
                }
                else if (algeb.length()==3)
                {
                    
                    target_algeb=san.substring(1,3);
                    
                    if((algeb.charAt(0)>='a')&&(algeb.charAt(0)<='z'))
                    {
                        file_algeb=algeb.charAt(0);
                        rank_algeb=' ';
                                                
                        m.from_algeb(file_algeb+"1"+target_algeb);
                    }
                    else
                    {
                        rank_algeb=algeb.charAt(0);
                        file_algeb=' ';
                        
                        m.from_algeb("a"+rank_algeb+target_algeb);
                    }
                    
                    san=san.substring(3);
                }
                else
                {
                    
                    m.from_algeb(algeb);
                    
                    if(san.length()>=4)
                    {
                        san=san.substring(4);
                    }
                    else
                    {
                        return m;
                    }
                }
                
                // disambiguation
                
                Piece search_piece=new Piece(piece);
                
                int piece_code=search_piece.code();
                
                int piece_type=piece_code&Piece.TYPE;
                
                Boolean is_sliding=((piece_type&Piece.SLIDING)!=0);
                
                int san_ptr=move_table_ptr[m.to.i][m.to.j][piece_code];
                
                MoveDescriptor md;
                
                Boolean found=false;
                
                if(turn==TURN_BLACK){search_piece.fen_char=search_piece.lower_fen_char();}
                
                do
                {
                    md=move_table[san_ptr];
                    
                    Piece to_piece=board[md.to_i][md.to_j];
                    
                    if(to_piece.empty())
                    {
                        san_ptr++;
                    }
                    else
                    {
                        if(search_piece.is_equal(to_piece))
                        {
                            Boolean file_match=true;
                            Boolean rank_match=true;
                            
                            if(file_algeb!=' ')
                            {
                                file_match=(md.to_i==m.from.i);
                            }
                            
                            if(rank_algeb!=' ')
                            {
                                rank_match=(md.to_j==m.from.j);
                            }
                            
                            if(file_match&&rank_match)
                            {
                                found=true;
                                m.from.i=md.to_i;
                                m.from.j=md.to_j;
                                
                                // check for check
                                
                                Board dummy_check_test=new Board();
            
                                dummy_check_test.set_from_fen(report_fen());

                                dummy_check_test.make_move(m);
                                
                                if(dummy_check_test.is_in_check(turn))
                                {
                                    found=false;
                                }
                                
                            }
                        }
                        
                        if(is_sliding)
                        {
                            san_ptr=md.next_vector;
                        }
                        else
                        {
                            san_ptr++;
                        }
                    }
                    
                }while((!found)&&(!md.end_piece));
                
            }
            
        }
        
        if(san.length()>1)
        {
            if(san.charAt(0)=='=')
            {
                // promotion
                if((variant!=VARIANT_ATOMIC)||(m.from.i==m.to.i))
                {
                    m.prom_piece=new Piece(san.charAt(1));
                }
            }
        }
        
        return m;
    }
    
    public String legal_sans[];
    public String legal_algebs[];
    public int legal_sans_cnt;
    
    public Boolean list_legal_moves(Move m)
    {
        
        legal_sans=new String[1000];
        legal_algebs=new String[1000];
        
        init_move_generator();
        legal_sans_cnt=0;
        Boolean found=false;
        while(next_pseudo_legal_move())
        {
            
            Board dummy=this.clone();
            
            dummy.make_move(current_move);
            
            Boolean legal=(!dummy.is_in_check(turn));
            
            if(current_move.to_algeb().equals("h4f2"))
            {
                int x=1;
            }
            
            if(variant==VARIANT_ATOMIC)
            {
                if(!legal)
                {
                    legal=((dummy.is_exploded(-turn))&&(!dummy.is_exploded(turn)));
                }
            }
            
            if(legal)
            {
                
                // move is legal
                
                if(m!=null)
                {
                    if(current_move.is_equal(m))
                    {
                        found=true;
                    }
                }
                
                String san=to_san(current_move);
                
                legal_sans[legal_sans_cnt]=san;
                legal_algebs[legal_sans_cnt]=current_move.to_algeb();
                
                legal_sans_cnt++;
                
            }
        }
        
        return found;
    }
    
    private String to_san_raw(Move m)
    {
        
        Piece from_piece=board[m.from.i][m.from.j].clone();
        
        String algeb=m.to_algeb();
        
        if(from_piece.type()==Piece.KING)
        {
            if(algeb.equals("e1g1")){return "O-O";}
            if(algeb.equals("e8g8")){return "O-O";}
            if(algeb.equals("e1c1")){return "O-O-O";}
            if(algeb.equals("e8c8")){return "O-O-O";}
        }
                
        Piece to_piece=board[m.to.i][m.to.j].clone();
        String target_algeb=""+algeb.charAt(2)+algeb.charAt(3);
        
        if(from_piece.type()==Piece.PAWN)
        {
            if(m.from.i==m.to.i)
            {
                // pawn push
                return target_algeb;
            }
            else
            {
                return algeb.charAt(0)+"x"+target_algeb;
            }
        }
        else
        {
            
            int test_ptr=move_table_ptr[m.to.i][m.to.j][from_piece.code()];
            
            MoveDescriptor md;
            
            Boolean ambiguity=false;
        
            Boolean same_rank=false;
            Boolean same_file=false;
            
            int from_rank_list[]=new int[50];
            int from_rank_cnt=0;
            int from_file_list[]=new int[50];
            int from_file_cnt=0;
            
            do
            {
                
                md=move_table[test_ptr];
                
                Piece to_piece_test=board[md.to_i][md.to_j].clone();
                
                if(to_piece_test.empty())
                {
                    test_ptr++;
                }
                else
                {
                    if((to_piece_test.is_equal(from_piece))&&((md.to_i!=m.from.i)||(md.to_j!=m.from.j)))
                    {
                        
                        Move test_move=new Move(new Square(md.to_i,md.to_j),new Square(m.to.i,m.to.j),new Piece(' '));
                        
                        Board dummy=new Board();
            
                        dummy.set_from_fen(report_fen());
            
                        dummy.make_move(test_move);
                        
                        if(!dummy.is_in_check(turn))                        
                        {
                            
                            ambiguity=true;
                            
                            from_rank_list[from_rank_cnt++]=md.to_j;
                            from_file_list[from_file_cnt++]=md.to_i;
                            
                            for(int r=0;r<from_rank_cnt;r++)
                            {
                                if(m.from.j==from_rank_list[r])
                                {
                                    same_rank=true;
                                }
                            }

                            for(int f=0;f<from_file_cnt;f++)
                            {
                                if(m.from.i==from_file_list[f])
                                {
                                    same_file=true;
                                }
                            }

                        
                        }
                        
                    }
                    
                    if((from_piece.type()&Piece.SLIDING)!=0)
                    {
                        test_ptr=md.next_vector;
                    }
                    else
                    {
                        test_ptr++;
                    }
                    
                }
                
            }while(!move_table[test_ptr].end_piece);
            
            String san=""+from_piece.upper_fen_char();
            
            if(ambiguity&&(!same_file)&&(!same_rank))
            {
                san+=algeb.charAt(0);
            }
            else
            {
                if(same_rank){san+=algeb.charAt(0);}
                if(same_file){san+=algeb.charAt(1);}
            }
            
            if(!to_piece.empty())
            {
                san+="x";
            }
            
            san+=target_algeb;
            
            return san;
            
        }
        
    }
    
    public String to_san(Move m)
    {
	
        String raw=to_san_raw(m);
        
        if(m.prom_piece.type()!=Piece.PIECE_NONE)
        {
            raw+="="+m.prom_piece.upper_fen_char();
        }
        
        Board dummy=new Board();
            
        dummy.set_from_fen(report_fen());
            
        dummy.make_move(m);
        
        Boolean is_check=dummy.is_in_check(dummy.turn);
        
        dummy.init_move_generator();
        
        Boolean has_legal=false;
        
        while((dummy.next_pseudo_legal_move())&&(!has_legal))
        {
            Board dummy2=new Board();
            
            dummy2.set_from_fen(dummy.report_fen());
            
            dummy2.make_move(dummy.current_move);
            
            if(!dummy2.is_in_check(dummy.turn))
            {
                has_legal=true;
            }
        }
        
        if(is_check)
        {
            if(has_legal)
            {
                raw+="+";
            }
            else
            {
                raw+="#";
            }
        }
        else if(!has_legal)
        {
            raw+="=";
        }
        
        return raw;
    }
    
    public void make_move(Move m)
    {
        
        // make move
        Piece orig_piece=board[m.from.i][m.from.j].clone();
        board[m.from.i][m.from.j].clear();
        
        Piece dest_piece=board[m.to.i][m.to.j];
        
        board[m.to.i][m.to.j]=orig_piece;
        
        Boolean is_ep_capture=((orig_piece.fen_char=='p')||(orig_piece.fen_char=='P'))&&(dest_piece.empty())&&(m.from.i!=m.to.i);
        
        Boolean is_capture=((is_ep_capture)||(!dest_piece.empty()));
        
        if((variant==VARIANT_ATOMIC)&&(is_capture))
        {
            
            board[m.to.i][m.to.j].clear();
            
            int capture_ptr=move_table_ptr[m.to.i][m.to.j][Piece.KING];
            
            MoveDescriptor md=move_table[capture_ptr++];
            
            while(!md.end_piece)
            {
                if((!md.castling)&&(board[md.to_i][md.to_j].type()!=Piece.PAWN))
                {
                    board[md.to_i][md.to_j].clear();
                }
                md=move_table[capture_ptr++];
            }
            
        }
        
        // clear ep
        ep_square=null;
        
        // promotion
        if(m.prom_piece.type()!=Piece.PIECE_NONE)
        {        
            board[m.to.i][m.to.j].from_fen_char(
                    m.prom_piece.fen_char_of_color(
                        turn==TURN_WHITE?Piece.WHITE:Piece.BLACK
                    )
            );
        }
        
        // turn
        turn=-turn;
        
        // halfmove clock
        
        Boolean is_pawn_move=(orig_piece.type()==Piece.PAWN);
        
        if(is_capture||is_pawn_move)
        {
            halfmove_clock=0;
        }
        else
        {
            halfmove_clock++;
        }
        
        // fullmove number
        if(turn==TURN_WHITE)
        {
            fullmove_number++;
        }
        
        // pawn push by two
        if(
                ((orig_piece.fen_char=='P')&&(m.from.j==6)&&(m.to.j==4))
                ||
                ((orig_piece.fen_char=='p')&&(m.from.j==1)&&(m.to.j==3))
        )
        {
            ep_square=new Square(m.from.i,m.from.j+(m.to.j-m.from.j)/2);
        }
        
        // castling rights
        
        if(orig_piece.fen_char=='k')
        {
            
            castling_rights=castling_rights&~CASTLE_k;
            castling_rights=castling_rights&~CASTLE_q;
            
        }
        
        if(orig_piece.fen_char=='K')
        {
            
            castling_rights=castling_rights&~CASTLE_K;
            castling_rights=castling_rights&~CASTLE_Q;
            
        }
        
        if(board[0][0].empty())
        {
            castling_rights=castling_rights&~CASTLE_q;
        }
        
        if(board[0][7].empty())
        {
            castling_rights=castling_rights&~CASTLE_Q;
        }
        
        if(board[7][0].empty())
        {
            castling_rights=castling_rights&~CASTLE_k;
        }
        
        if(board[7][7].empty())
        {
            castling_rights=castling_rights&~CASTLE_K;
        }
        
        // castling
        if(orig_piece.fen_char=='k')
        {
            if((m.from.j==0)&&(m.from.i==4)&&(m.to.j==0)&&(m.to.i==6))
            {
                board[7][0].fen_char=' ';
                board[5][0].fen_char='r';
            }

            if((m.from.j==0)&&(m.from.i==4)&&(m.to.j==0)&&(m.to.i==2))
            {
                board[0][0].fen_char=' ';
                board[3][0].fen_char='r';
            }
        }
        
        if(orig_piece.fen_char=='K')
        {
            if((m.from.j==7)&&(m.from.i==4)&&(m.to.j==7)&&(m.to.i==6))
            {
                board[7][7].fen_char=' ';
                board[5][7].fen_char='R';
            }

            if((m.from.j==7)&&(m.from.i==4)&&(m.to.j==7)&&(m.to.i==2))
            {
                board[0][7].fen_char=' ';
                board[3][7].fen_char='R';
            }
        }
        
        // ep capture
        if(is_ep_capture)
        {
            board[m.to.i][m.from.j].clear();
            
            if(variant==VARIANT_ATOMIC)
            {
                board[m.to.i][m.to.j].clear();
            }
        }
        
    }
    
    /////////////////////////////////////////////
    // definitions
    
    final static char promotion_pieces[]={'q','r','b','n'};
    
    final static int move_table_size=20000;
    static MoveDescriptor move_table[]=new MoveDescriptor[move_table_size];
    static int move_table_ptr[][][]=new int[8][8][64];
    
    final static int CASTLE_K=8;
    final static int CASTLE_Q=4;
    final static int CASTLE_W=CASTLE_K|CASTLE_Q;
    final static int CASTLE_k=2;
    final static int CASTLE_q=1;
    final static int CASTLE_b=CASTLE_k|CASTLE_q;
    
    final static int TURN_WHITE=1;
    final static int TURN_BLACK=-1;
    
    /////////////////////////////////////////////
    // fen fields
    
    public Piece[][] board=new Piece[8][8];
    
    public int turn=TURN_WHITE;
    
    public int castling_rights=0;
    
    public Square ep_square=new Square();
    
    public int halfmove_clock=0;
    
    public int fullmove_number=1;
    
    /////////////////////////////////////////////
    
    private static Boolean square_ok(int i,int j)
    {
        if((i>=0)&&(i<=7)&&(j>=0)&&(j<=7))
        {
            return true;
        }
        return false;
    }
    
    public static void init_move_table()
    {
        int move_table_curr_ptr=0;
        
        for(int i=0;i<8;i++)
        {
            for(int j=0;j<8;j++)
            {
                for(int p=0;p<64;p++)
                {
                    int piece_type=p&Piece.TYPE;
                    int piece_color=p&Piece.COLOR;
                    
                    if((piece_type==Piece.QUEEN)||(piece_type==Piece.ROOK)||(piece_type==Piece.BISHOP)||(piece_type==Piece.KNIGHT)||(piece_type==Piece.KING)||(piece_type==Piece.PAWN))
                    {
                        
                        Boolean is_single=((piece_type&Piece.SINGLE)!=0);
                        
                        move_table_ptr[i][j][p]=move_table_curr_ptr;
                        
                        for(int vi=-2;vi<=2;vi++)
                        {
                            for(int vj=-2;vj<=2;vj++)
                            {
                                
                                Boolean is_castling=(
                                            (
                                                // castling white
                                                (p==(Piece.WHITE|Piece.KING))
                                                &&
                                                (
                                                    ((i==4)&&(j==7)&&(vi==2)&&(vj==0))
                                                    ||
                                                    ((i==4)&&(j==7)&&(vi==-2)&&(vj==0))
                                                )
                                            )
                                        
                                            ||
                                        
                                            (
                                                // castling black
                                                (p==(Piece.BLACK|Piece.KING))
                                                &&
                                                (
                                                    ((i==4)&&(j==0)&&(vi==2)&&(vj==0))
                                                    ||
                                                    ((i==4)&&(j==0)&&(vi==-2)&&(vj==0))
                                                )
                                            )
                                );
                                
                                if(
                                        
                                        // cannot be both zero
                                        ((Math.abs(vi)+Math.abs(vj))>0)
                                        
                                        &&
                                        
                                        (
                                        
                                            (is_castling)
                                        
                                            ||
                                        
                                            (
                                                ((vi*vj)!=0)
                                                &&
                                                ((Math.abs(vi)!=2)&&(Math.abs(vj)!=2))
                                                &&
                                                ((piece_type&Piece.DIAGONAL)!=0)
                                            )
                                        
                                            ||
                                        
                                            (
                                                ((vi*vj)==0)
                                                &&
                                                ((Math.abs(vi)!=2)&&(Math.abs(vj)!=2))
                                                &&
                                                ((piece_type&Piece.STRAIGHT)!=0)
                                            )
                                        
                                            ||
                                        
                                            (
                                                (Math.abs(vi*vj)==2)
                                                &&
                                                (piece_type==Piece.KNIGHT)
                                            )
                                        
                                            ||
                                        
                                            (
                                                (piece_type==Piece.PAWN)
                                                &&
                                                (Math.abs(vi)<2)
                                                &&
                                                (Math.abs(vj)>0)
                                                &&
                                                (
                                                    (
                                                        (piece_color==Piece.WHITE)
                                                        &&
                                                        (vj<0)
                                                        &&
                                                        (
                                                            (Math.abs(vj)==1)
                                                            ||
                                                            (
                                                                (j==6)
                                                                &&
                                                                (vi==0)
                                                            )
                                                        )
                                                    )
                                                    ||
                                                    (
                                                        (piece_color==Piece.BLACK)
                                                        &&
                                                        (vj>0)
                                                        &&
                                                        (
                                                            (Math.abs(vj)==1)
                                                            ||
                                                            (
                                                                (j==1)
                                                                &&
                                                                (vi==0)
                                                            )
                                                        )
                                                    )
                                                )
                                            )
                                            
                                        )
                                        
                                )
                                {
                                    
                                    int start_vector=move_table_curr_ptr;
                                    
                                    int ci=i;
                                    int cj=j;
                                    
                                    Boolean square_ok;
                                    
                                    do
                                    {
                                        
                                        ci+=vi;
                                        cj+=vj;
                                        
                                        square_ok=square_ok(ci,cj);
                                        
                                        if(square_ok)
                                        {
                                            if(
                                                    ((p==(Piece.WHITE|Piece.PAWN))&&(cj==0))
                                                    ||
                                                    ((p==(Piece.BLACK|Piece.PAWN))&&(cj==7))
                                            )
                                            {
                                                for(int prom=0;prom<promotion_pieces.length;prom++)
                                                {
                                                    MoveDescriptor md=new MoveDescriptor();
                                                    md.to_i=ci;
                                                    md.to_j=cj;
                                                    md.castling=false;
                                                    md.promotion=true;
                                                    md.prom_piece=promotion_pieces[prom];

                                                    move_table[move_table_curr_ptr++]=md;
                                                }
                                            }
                                            else
                                            {
                                                MoveDescriptor md=new MoveDescriptor();
                                                md.to_i=ci;
                                                md.to_j=cj;
                                                md.castling=is_castling;

                                                move_table[move_table_curr_ptr++]=md;
                                            }
                                        }
                                             
                                    }while(square_ok&&(!is_single));
                                    
                                    for(int ptr=start_vector;ptr<move_table_curr_ptr;ptr++)
                                    {
                                        move_table[ptr].next_vector=move_table_curr_ptr;
                                    }
                                    
                                    
                                }
                            }
                        }
                        
                        // piece finished
                        
                        move_table[move_table_curr_ptr]=new MoveDescriptor();
                        move_table[move_table_curr_ptr++].end_piece=true;
                        
                    }
                    
                }
                
            }
            
        }
        
        //System.out.println("move table initialized, number of entries: "+move_table_curr_ptr);
    }
    
    public String turn_as_string()
    {
        return turn==TURN_WHITE?"w":"b";
    }
    
    public String castling_rights_as_string()
    {
        String castling_rights_as_string="";
        castling_rights_as_string+=((castling_rights&CASTLE_K)!=0)?"K":"";
        castling_rights_as_string+=((castling_rights&CASTLE_Q)!=0)?"Q":"";
        castling_rights_as_string+=((castling_rights&CASTLE_k)!=0)?"k":"";
        castling_rights_as_string+=((castling_rights&CASTLE_q)!=0)?"q":"";
        if(castling_rights_as_string.length()<1)
        {
            return "-";
        }
        return castling_rights_as_string;
    }
    
    public String ep_square_as_string()
    {
        if(ep_square==null)
        {
            return "-";
        }
        
        String ep_square_as_string=ep_square.to_algeb();
        
        return ep_square_as_string;
    }
    
    public String as_string()
    {
        String board_as_string="";
        for(int j=0;j<8;j++)
        {
            for(int i=0;i<8;i++)
            {
                board_as_string+=board[i][j].empty()?".":board[i][j].fen_char;
            }
            board_as_string+="\n";
        }
        
        board_as_string+=
                "\n"+
                "turn = "+turn_as_string()+
                " , castling rights = "+castling_rights_as_string()+
                " , ep square = "+ep_square_as_string()+
                " , halfmove clock = "+halfmove_clock+
                " , fullmove number = "+fullmove_number+
                "\n"
                ;
        
        return board_as_string;
    }
    
    public void print()
    {
        System.out.print(as_string());
    }
    
    public Boolean set_from_fen(String fen)
    {
        Boolean result=set_from_fen_unchecked(fen);
        
        if(!check_sanity())
        {
            return false;
        }
        
        return result;
    }
    
    public Boolean set_from_fen_unchecked(String fen)
    {
        MyTokenizer fen_tokenizer=new MyTokenizer(fen);
        
        String pieces=fen_tokenizer.get_token();
        
        if(pieces==null)
        {
            return null;
        }
        
        MyTokenizer pieces_tokenizer=new MyTokenizer(pieces);
        
        int i=0;
        
        int j=0;
        
        String fen_char_str;
                
        do
        {
            fen_char_str=pieces_tokenizer.get_char();
            
            if(fen_char_str!=null)
            {
                char fen_char=fen_char_str.charAt(0);
                
                if(fen_char=='/')
                {
                    continue;
                }
                
                int repeat=1;
                if((fen_char>='1')&&(fen_char<='8'))
                {
                    repeat=(int)(fen_char-'0');
                    fen_char=' ';
                }
                for(int r=0;r<repeat;r++)
                {
                    board[i][j].from_fen_char_safe(fen_char);
                    i++;
                    if(i>7)
                    {
                        i=0;
                        j++;
                    }
                    if(j>7)
                    {
                        break;
                    }
                }
            }
        }while((fen_char_str!=null)&&(j<8));
        
        String turn_string=fen_tokenizer.get_token();
        
        if(turn_string==null)
        {
            return false;
        }
        
        turn=turn_string.equals("w")?TURN_WHITE:TURN_BLACK;
        
        String castling_rights_string=fen_tokenizer.get_token();
        
        if(castling_rights_string==null)
        {
            return false;
        }
        
        ////////////////////////////////////////////////////////////////////////
        
        set_castling_rights_from_string(castling_rights_string);
        
        ////////////////////////////////////////////////////////////////////////
        
        String ep_square_string=fen_tokenizer.get_token();
        
        if(ep_square_string==null)
        {
            return false;
        }
        
        if(ep_square_string.equals("-"))
        {
            ep_square=null;
        }
        else
        {
            ep_square=new Square();
            ep_square.from_algeb(ep_square_string);
            if(!ep_square.valid)
            {
                ep_square=null;
            }
        }
        
        String halfmove_clock_string=fen_tokenizer.get_token();
        
        if(halfmove_clock_string==null)
        {
            return false;
        }
        
        if(halfmove_clock_string.matches("^[0-9]+$"))
        {
            halfmove_clock=Integer.parseInt(halfmove_clock_string);
        }
        
        String fullmove_number_string=fen_tokenizer.get_token();
        
        if(fullmove_number_string==null)
        {
            return false;
        }
        
        if(fullmove_number_string.matches("^[0-9]+$"))
        {
            fullmove_number=Integer.parseInt(fullmove_number_string);
        }
        
        return true;
    }
    
    public void set_castling_rights_from_string(String castling_rights_string)
    {
        castling_rights=0;
        for(int c=0;c<castling_rights_string.length();c++)
        {
            switch(castling_rights_string.charAt(c))
            {
                case 'K':castling_rights|=CASTLE_K;break;
                case 'Q':castling_rights|=CASTLE_Q;break;
                case 'k':castling_rights|=CASTLE_k;break;
                case 'q':castling_rights|=CASTLE_q;break;
            }
        }
    }
    
    public Boolean check_sanity()
    {
        Boolean is_white_king_orig=(board[4][7].is_equal(new Piece('K')));
        Boolean is_black_king_orig=(board[4][0].is_equal(new Piece('k')));
        
        int disable=0;
        
        if((castling_rights&CASTLE_K)!=0)
        {
            if((!is_white_king_orig)||(!board[7][7].is_equal(new Piece('R'))))
            {
                disable|=CASTLE_K;
            }
        }
        
        if((castling_rights&CASTLE_Q)!=0)
        {
            if((!is_white_king_orig)||(!board[0][7].is_equal(new Piece('R'))))
            {
                disable|=CASTLE_Q;
            }
        }
        
        if((castling_rights&CASTLE_k)!=0)
        {
            if((!is_black_king_orig)||(!board[7][0].is_equal(new Piece('r'))))
            {
                disable|=CASTLE_k;
            }
        }
        
        if((castling_rights&CASTLE_q)!=0)
        {
            if((!is_black_king_orig)||(!board[0][0].is_equal(new Piece('r'))))
            {
                disable|=CASTLE_q;
            }
        }
        
        castling_rights&=(~disable);
        
        return true;
    }
    
    public String report_fen()
    {
        check_sanity();
        
        return report_fen_unchecked();
    }
    
    public String report_fen_unchecked()
    {
        String fen="";
        
        int repeat=0;
        
        for(int j=0;j<8;j++)
        {
            for(int i=0;i<8;i++)
            {
                char fen_char=board[i][j].fen_char;
                if(fen_char==' ')
                {
                    repeat++;
                    if(i==7)
                    {
                        fen+=repeat;
                        repeat=0;
                    }
                }
                else
                {
                    if(repeat>0)
                    {
                        fen+=repeat;
                        repeat=0;
                    }
                    fen+=fen_char;
                }
            }
            
            if(j<7)
            {
                fen+='/';
            }
        }
        
        fen+=" "+
                turn_as_string()+" "+
                castling_rights_as_string()+" "+
                ep_square_as_string()+" "+
                halfmove_clock+" "+
                fullmove_number;
        
        return fen;
    }
    
    static public String start_fen()
    {
        return "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
    }
    
    public void reset()
    {
        set_from_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1");
    }
    
    public void copy(Board what)
    {
        for(int i=0;i<8;i++)
        {
            for(int j=0;j<8;j++)
            {
                board[i][j].fen_char=what.board[i][j].fen_char;
            }
        }
    
        turn=what.turn;
    
        castling_rights=what.castling_rights;
    
        ep_square=what.ep_square;
    
        halfmove_clock=what.halfmove_clock;
    
        fullmove_number=what.fullmove_number;
    }
    
    public String report_status()
    {
        list_legal_moves(null);
        if(legal_sans_cnt<=0)
        {
            if(is_in_check(turn))
            {
                return "mate";
            }
            return "stalemate";
        }
        return "";
    }
    
    public Board clone()
    {
        Board dummy=new Board();
        dummy.copy(this);
        return dummy;
    }
    
    public Boolean set_from_fen_safe(String fen)
    {
        Board dummy=clone();
        Boolean result_ok=dummy.set_from_fen(fen);
        if(result_ok)
        {
            copy(dummy);
        }
        return result_ok;
    }
    
    public Board()
    {
        for(int i=0;i<8;i++)
        {
            for(int j=0;j<8;j++)
            {
                board[i][j]=new Piece();
            }
        }
    }
            
}
