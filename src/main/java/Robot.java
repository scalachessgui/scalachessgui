package gui2;

import java.awt.*;
import java.awt.image.*;
import javafx.embed.swing.*;
import javafx.scene.image.*;

public class Robot {

	public static java.awt.Robot getRobot()
	{
		java.awt.Robot r=null;
		try
		{
			r=new java.awt.Robot();
			return(r);
		}
		catch(java.awt.AWTException ex)
		{
			return(null);
		}
	}

	public static java.awt.Robot r=getRobot();
	
	public static BufferedImage bimage=null;
    
    public static WritableImage wimage=null;

	public static void click_xy(int x,int y)
	{
	
		java.awt.PointerInfo pi=java.awt.MouseInfo.getPointerInfo();
		
		java.awt.Point p=pi.getLocation();
		
		r.mouseMove(x, y);
		r.mousePress(java.awt.event.InputEvent.BUTTON1_MASK);
		try { Thread.sleep(100); } catch (InterruptedException ex) {}
		r.mouseRelease(java.awt.event.InputEvent.BUTTON1_MASK);
		
		r.mouseMove(p.x,p.y);
		
	}
    
    public static BufferedImage getImage(int x0,int y0,int w,int h)
    {
        bimage=r.createScreenCapture(new Rectangle(x0,y0,w,h));
        
        //wimage=SwingFXUtils.toFXImage(bimage, null);
        
        return bimage;
    }

}