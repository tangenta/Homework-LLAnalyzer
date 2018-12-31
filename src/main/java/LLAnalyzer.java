import analyzer.*;
import scala.collection.immutable.List;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.nio.file.Files;
import java.util.stream.Collectors;

public class LLAnalyzer {
    private JPanel panel;
    private JButton mRemoveRecursionButton;
    private JEditorPane grammarPane;
    private JButton mRemoveFactorButton;
    private JCheckBox mRecursionCheckBox;
    private JCheckBox mFactorCheckBox;
    private JButton mOpenButton;
    private JButton mSaveAsButton;
    private JCheckBox mIsLL1CheckBox;
    private JButton mParseButton;
    private JTextField mTermTextField;
    private JTextArea mResultTextArea;
    private JTextArea mTableTextArea;
    private JButton mUpdateStatusButton;
    private JCheckBox mSyntaxErrCheckBox;
    private JCheckBox mAutoUpdateCheckBox;

    private JFileChooser chooser = new JFileChooser();

    private Timer mTimer;

    private static final Algorithm algo = new Algorithm();

    private List<Production> productions;
    private LLTable table;
    private String tableStr;

    // ================ status ===============
    private boolean isLeftRecursive = false;
    private boolean hasLeftFactor = false;
    private boolean isLL1Grammar = false;

    volatile private boolean grammarHasParsed = false;


    public LLAnalyzer() {

        setCheckBoxUnselectable();

        mTimer = new Timer(2000, e -> {
            if (!grammarHasParsed) {
                calculateTable();
            }
        });
        mTimer.start();
        grammarPane.addKeyListener(new KeyAdapter() {
            @Override
            public void keyTyped(KeyEvent e) {
                super.keyTyped(e);
                grammarHasParsed = false;
                if (mTimer.isRunning()) mTimer.restart();
            }
        });
        mAutoUpdateCheckBox.addActionListener(e -> {
            if (mAutoUpdateCheckBox.isSelected()) mTimer.restart();
            else mTimer.stop();
        });

        mUpdateStatusButton.addActionListener(e -> calculateTable());

        mRemoveRecursionButton.addActionListener(e -> {
            productions = Adapter.eliminateLeftRecursion(productions);
            grammarPane.setText(Adapter.stringify(productions));
            calculateTable();
        });

        mRemoveFactorButton.addActionListener(e -> {
            productions = Adapter.eliminateLeftFactor(productions);
            grammarPane.setText(Adapter.stringify(productions));
            calculateTable();
        });

        mParseButton.addActionListener(e -> {
            String parseResult = Adapter.parse(table, Adapter.buildTerms(mTermTextField.getText()));
            mResultTextArea.setText(parseResult == null ? "Parsing error." : parseResult + "\n\nParsed successful.");
        });
        mOpenButton.addActionListener(e -> {
            int returnVal = chooser.showOpenDialog(panel);
            if (returnVal == JFileChooser.APPROVE_OPTION) {
                File file = chooser.getSelectedFile();
                try {
                    String text = Files.lines(file.toPath()).collect(Collectors.joining(System.lineSeparator()));
                    grammarPane.setText(text);
                } catch (IOException e1) {
                    e1.printStackTrace();
                }

            }
        });
        mSaveAsButton.addActionListener(e -> {
            int returnVal = chooser.showSaveDialog(panel);
            if (returnVal == JFileChooser.APPROVE_OPTION) {
                File file = chooser.getSelectedFile();
                try (Writer writer = new BufferedWriter(new FileWriter(file))) {
                    writer.write(grammarPane.getText());
                } catch (IOException e1) {
                    e1.printStackTrace();
                }
            }
        });

        calculateTable();
    }

    public static void main(String[] args) {
        try {
            UIManager.setLookAndFeel("com.sun.java.swing.plaf.gtk.GTKLookAndFeel");
        } catch (Exception ignored) {}
        SwingUtilities.invokeLater(() -> {
            JFrame frame = new JFrame("LLAnalyzer");
            frame.setContentPane(new LLAnalyzer().panel);
            frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
            Dimension dimension = java.awt.Toolkit.getDefaultToolkit().getScreenSize();
            frame.setSize(dimension.width/2, dimension.height/2);
            frame.setVisible(true);
        });
    }

    private void calculateTable() {
        grammarHasParsed = true;
        boolean isUiThread = SwingUtilities.isEventDispatchThread();
        try {
            productions = GrammarScanner.parseProductions(grammarPane.getText());
            if (productions.isEmpty()) throw new RuntimeException();
        } catch (RuntimeException e) {
            productions = null;
            wrap(this::updateUI, isUiThread);
            return;
        }
        hasLeftFactor = Adapter.hasLeftCommonFactor(productions);
        isLeftRecursive = Adapter.isLeftRecursive(productions);

        if (!hasLeftFactor && !isLeftRecursive) {
            table = new LLTable(productions, algo, productions.head().head());
            tableStr = Adapter.stringify(table);
        } else {
            table = null;
            tableStr = null;
        }
        wrap(this::updateUI, isUiThread);
    }

    private void updateUI() {
        if (productions == null) {      // has syntax error in grammar
            mRemoveFactorButton.setEnabled(false);
            mRemoveRecursionButton.setEnabled(false);
            mParseButton.setEnabled(false);

            mRecursionCheckBox.setEnabled(false);
            mRecursionCheckBox.setSelected(false);
            mFactorCheckBox.setEnabled(false);
            mFactorCheckBox.setSelected(false);
            mSyntaxErrCheckBox.setSelected(true);
            mIsLL1CheckBox.setEnabled(false);
            mIsLL1CheckBox.setSelected(false);
            return;
        }

        mSyntaxErrCheckBox.setSelected(false);

        mRemoveRecursionButton.setEnabled(isLeftRecursive);
        mRemoveFactorButton.setEnabled(hasLeftFactor);

        mRecursionCheckBox.setEnabled(true);
        mRecursionCheckBox.setSelected(isLeftRecursive);
        mFactorCheckBox.setEnabled(true);
        mFactorCheckBox.setSelected(hasLeftFactor);

        if (table == null) {
            mParseButton.setEnabled(false);
            mIsLL1CheckBox.setEnabled(false);
            mIsLL1CheckBox.setSelected(false);
            return;
        }


        if (!table.isLL1Table()) {
            mParseButton.setEnabled(false);
            mIsLL1CheckBox.setEnabled(true);
            mIsLL1CheckBox.setSelected(false);
            return;
        }

        isLL1Grammar = true;
        mParseButton.setEnabled(true);
        mIsLL1CheckBox.setEnabled(true);
        mIsLL1CheckBox.setSelected(table.isLL1Table());
        mTableTextArea.setText(tableStr);
    }


    private void wrap(Runnable runnable, boolean isUiThread) {
        if (isUiThread) runnable.run();
        else SwingUtilities.invokeLater(runnable);
    }


    // === invoke once ===
    private void setCheckBoxUnselectable() {
        mSyntaxErrCheckBox.addActionListener(e -> mSyntaxErrCheckBox.setSelected(productions == null));
        mRecursionCheckBox.addActionListener(e -> mRecursionCheckBox.setSelected(isLeftRecursive));
        mFactorCheckBox.addActionListener(e -> mFactorCheckBox.setSelected(hasLeftFactor));
        mIsLL1CheckBox.addActionListener(e -> mIsLL1CheckBox.setSelected(isLL1Grammar));
    }
}
