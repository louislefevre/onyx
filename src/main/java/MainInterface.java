import javafx.application.Application;
import javafx.geometry.Orientation;
import javafx.geometry.Rectangle2D;
import javafx.scene.Scene;
import javafx.scene.control.*;
import javafx.scene.layout.Background;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import javafx.scene.paint.Color;
import javafx.scene.text.Font;
import javafx.scene.text.FontWeight;
import javafx.scene.text.Text;
import javafx.stage.FileChooser;
import javafx.stage.Screen;
import javafx.stage.Stage;

import java.io.File;

public class MainInterface extends Application
{
    public static void main(String[] args)
    {
        launch(args);
    }

    @Override
    public void start(Stage primaryStage)
    {
        Rectangle2D screenBounds = Screen.getPrimary().getBounds();
        double width = screenBounds.getWidth() * 0.4;
        double height = screenBounds.getHeight() * 0.75;

        primaryStage.setTitle("Onyx Compiler");

        MenuBar menuBar = new MenuBar();
        Menu menu = new Menu("Open");
        menuBar.getMenus().add(menu);

        MenuItem openFile = new MenuItem("File");
        FileChooser fileChooser = new FileChooser();
        openFile.setOnAction(e -> {
            File selectedFile = fileChooser.showOpenDialog(primaryStage);
        });

        menu.getItems().add(openFile);

        TextArea textArea = new TextArea();
        textArea.setMinHeight(height*0.8);
        textArea.setWrapText(true);

        Text text = new Text();
        text.setFill(Color.RED);
        text.setFont(Font.font("Monospaced Regular"));
        text.setWrappingWidth(width);
        text.setText("This is the text to displayThis is the text to displayThis is the text to displayThis is the text to display displayThis is the text to display");

        System.out.println(Font.font("Monospaced Regular"));

        SplitPane splitPane = new SplitPane();

        VBox vboxTop = new VBox(menuBar, textArea);
        VBox vboxBottom = new VBox(text);
        vboxBottom.setBackground(Background.EMPTY);
        String style = "-fx-background-color: rgba(0, 0, 0, 0.9);";
        vboxBottom.setStyle(style);

        splitPane.getItems().addAll(vboxTop, vboxBottom);
        splitPane.setOrientation(Orientation.VERTICAL);

        Scene scene = new Scene(splitPane, width, height);
        primaryStage.setScene(scene);
        primaryStage.show();


//        primaryStage.setTitle("JavaFX App");
//
//        ToolBar toolBar = new ToolBar();
//        Button button = new Button("Open File");
//        toolBar.getItems().add(button);
//        FileChooser fileChooser = new FileChooser();
//        button.setOnAction(e -> {
//            File selectedFile = fileChooser.showOpenDialog(primaryStage);
//        });
//        VBox vBox = new VBox(toolBar);
//
//        Scene scene = new Scene(vBox, 960, 600);
//        primaryStage.setScene(scene);
//        primaryStage.show();
    }
}
