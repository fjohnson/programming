package ezmount;

import java.awt.BorderLayout;
import java.awt.EventQueue;

import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.border.EmptyBorder;
import com.jgoodies.forms.layout.FormLayout;
import com.jgoodies.forms.layout.ColumnSpec;
import com.jgoodies.forms.layout.RowSpec;
import com.jgoodies.forms.factories.FormFactory;
import javax.swing.JLabel;
import javax.swing.JTextField;
import javax.swing.JButton;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

public class SshMountQuery extends JFrame {

	private JPanel mountPathFrame;
	private JTextField textField;
	private SshMounter sm;
	/**
	 * Launch the application.
	 */
	public static void main(String[] args) {
		EventQueue.invokeLater(new Runnable() {
			public void run() {
				try {
					SshMountQuery frame = new SshMountQuery();
					frame.setVisible(true);
				} catch (Exception e) {
					e.printStackTrace();
				}
			}
		});
	}

	public SshMountQuery(SshMounter sm){
		this();
		this.sm = sm;
	}

	/**
	 * Create the frame.
	 */
	public SshMountQuery() {
		setTitle("New Mount Path");
		setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		setBounds(100, 100, 450, 122);
		mountPathFrame = new JPanel();
		mountPathFrame.setBorder(new EmptyBorder(5, 5, 5, 5));
		setContentPane(mountPathFrame);
		mountPathFrame.setLayout(new FormLayout(new ColumnSpec[] {
				FormFactory.RELATED_GAP_COLSPEC,
				ColumnSpec.decode("max(42dlu;default):grow"),},
			new RowSpec[] {
				FormFactory.RELATED_GAP_ROWSPEC,
				FormFactory.DEFAULT_ROWSPEC,
				FormFactory.RELATED_GAP_ROWSPEC,
				FormFactory.DEFAULT_ROWSPEC,
				FormFactory.RELATED_GAP_ROWSPEC,
				FormFactory.DEFAULT_ROWSPEC,}));
		
		JLabel MountPathLabel = new JLabel("Mount Path i.e fjohnson@ninja:~/");
		mountPathFrame.add(MountPathLabel, "2, 2");
		
		textField = new JTextField();
		mountPathFrame.add(textField, "2, 4, fill, default");
		textField.setColumns(10);
		
		JButton AddPath = new JButton("Ok");
		AddPath.addMouseListener(new MouseAdapter() {
			@Override
			public void mouseClicked(MouseEvent e) {
				SshMountQuery.this.setVisible(false);
				sm.newMountPoint(textField.getText());
				sm.setVisible(true);
			}
		});
		mountPathFrame.add(AddPath, "2, 6, left, default");
	}

	String getMountPath(){
		System.out.println(textField.getText());
		return textField.getText();
	}
}
