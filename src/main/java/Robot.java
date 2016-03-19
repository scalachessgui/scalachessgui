package gui2;

public class Robot {

	public static void click_xy(int x,int y)
	{
		
		java.awt.Robot r=null;
		try
		{
			r=new java.awt.Robot();
		}
		catch(java.awt.AWTException ex)
		{
			return;
		}
		
		java.awt.PointerInfo pi=java.awt.MouseInfo.getPointerInfo();
		
		java.awt.Point p=pi.getLocation();
		
		r.mouseMove(x, y);
		r.mousePress(java.awt.event.InputEvent.BUTTON1_MASK);
		try { Thread.sleep(100); } catch (InterruptedException ex) {}
		r.mouseRelease(java.awt.event.InputEvent.BUTTON1_MASK);
		
		r.mouseMove(p.x,p.y);
		
	}

}