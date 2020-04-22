package ui;

import javafx.application.Application;
import javafx.geometry.Orientation;
import javafx.scene.Scene;
import javafx.scene.control.SplitPane;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.VBox;
import javafx.stage.Screen;
import javafx.stage.Stage;

public final class PrimaryInterface extends Application
{
    private static final String TITLE = "Onyx Compiler";

    public void run(String[] args)
    {
        Application.launch(args);
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
        MenuBox menuBox = new MenuBox();
        InputBox inputBox = new InputBox();
        OutputBox outputBox = new OutputBox();

        VBox menu = menuBox.getBox();
        VBox top = inputBox.getBox();
        VBox bottom = outputBox.getBox();

        top.setMinHeight(getWindowHeight() * 0.7);
        bottom.setMinHeight(getWindowHeight() * 0.3);

        SplitPane splitPane = new SplitPane(top, bottom);
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
