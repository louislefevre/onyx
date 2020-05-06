package ui;

import javafx.application.Application;
import javafx.geometry.Orientation;
import javafx.scene.Scene;
import javafx.scene.control.MenuBar;
import javafx.scene.control.SplitPane;
import javafx.scene.layout.BorderPane;
import javafx.scene.text.TextFlow;
import javafx.stage.Screen;
import javafx.stage.Stage;
import org.fxmisc.flowless.VirtualizedScrollPane;

public final class PrimaryInterface extends Application
{
    private static final String TITLE = "Onyx Compiler";

    public void launchInterface()
    {
        Application.launch();
    }

    @Override
    public void start(Stage primaryStage)
    {
        primaryStage.setTitle(TITLE);
        primaryStage.setWidth(getWindowWidth());
        primaryStage.setHeight(getWindowHeight());
        primaryStage.setScene(createScene());
        primaryStage.show();
    }

    private Scene createScene()
    {
        InputBox inputBox = new InputBox();
        OutputBox outputBox = new OutputBox();
        MenuBox menuBox = new MenuBox(inputBox, outputBox);

        MenuBar menu = menuBox.getMenuBar();
        VirtualizedScrollPane input = inputBox.getScrollPane();
        TextFlow output = outputBox.getTextFlow();

        input.setPrefHeight(getWindowHeight() * 0.7);
        output.setPrefHeight(getWindowHeight() * 0.3);

        SplitPane splitPane = new SplitPane(input, output);
        splitPane.setDividerPosition(0, 0.7);
        splitPane.setOrientation(Orientation.VERTICAL);

        BorderPane borderPane = new BorderPane(splitPane);
        borderPane.setTop(menu);

        return new Scene(borderPane);
    }

    public static double getWindowWidth()
    {
        return Screen.getPrimary().getBounds().getWidth() * 0.4;
    }

    public static double getWindowHeight()
    {
        return Screen.getPrimary().getBounds().getHeight() * 0.75;
    }
}
