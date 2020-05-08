package ui;

import javafx.stage.FileChooser;
import javafx.stage.Stage;
import lombok.Getter;
import lombok.Setter;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.PrintWriter;
import java.nio.file.Files;

@Getter
@Setter
final class FileManager
{
    private File currentFile;
    private File initialDirectory;
    private String[] validFileExtensions;

    public FileManager(File initialDirectory, String... validFileExtensions)
    {
        this.currentFile = null;
        this.initialDirectory = initialDirectory;
        this.validFileExtensions = validFileExtensions;
    }

    public FileManager()
    {
        this(new File(System.getProperty("user.home"), "/Documents"), ".txt", ".*");
    }

    public String openFile() throws IOException, NullPointerException, IllegalArgumentException
    {
        FileChooser fileChooser = new FileChooser();
        addInitialDirectory(fileChooser);
        addExtensionFilters(fileChooser);

        File file = fileChooser.showOpenDialog(new Stage());
        if (file == null)
            throw new NullPointerException("No file was selected");
        else if (isInvalidFileExtension(file))
            throw new IllegalArgumentException("Invalid file extension");
        else if (isValidReadFile(file))
            return readFile(file);
        else
            throw new FileNotFoundException("Selected file doesn't exist: " + file.getPath());
    }

    public void saveFileAs(String text) throws FileNotFoundException
    {
        String initialFileName = "new-script.txt";
        saveFileAs(text, initialFileName);
    }

    public void saveFileAs(String text, String initialFileName) throws FileNotFoundException, NullPointerException, IllegalArgumentException
    {
        FileChooser fileChooser = new FileChooser();
        addInitialDirectory(fileChooser);
        addExtensionFilters(fileChooser);
        fileChooser.setInitialFileName(initialFileName);

        File file = fileChooser.showSaveDialog(new Stage());
        if (file == null)
            throw new NullPointerException("No file was selected");
        else if (!isValidFileExtension(file))
            throw new IllegalArgumentException("Invalid file extension");
        else
            writeFile(file, text);
    }

    public void saveFile(String text) throws FileNotFoundException
    {
        File file = currentFile;
        if (isValidWriteFile(file))
            writeFile(file, text);
        else
            saveFileAs(text);
    }

    private void addInitialDirectory(FileChooser fileChooser)
    {
        if (initialDirectory.exists())
            fileChooser.setInitialDirectory(initialDirectory);
        else
            System.out.println("Failed to find initial directory: " + initialDirectory);
    }

    private void addExtensionFilters(FileChooser fileChooser)
    {
        for (String extension : validFileExtensions)
        {
            fileChooser.getExtensionFilters().add(
                    new FileChooser.ExtensionFilter(extension, "*" + extension)
            );
        }
    }

    private String readFile(File file) throws IOException
    {
        String fileText = Files.readString(file.toPath());
        currentFile = file;
        return fileText;
    }

    private void writeFile(File file, String text) throws FileNotFoundException
    {
        PrintWriter writer = new PrintWriter(file);
        writer.print(text);
        writer.close();
        currentFile = file;
    }

    private boolean isValidReadFile(File file)
    {
        return isValidFile(file) && file.canRead();
    }

    private boolean isValidWriteFile(File file)
    {
        return isValidFile(file) && file.canWrite();
    }

    private boolean isValidFile(File file)
    {
        return file != null && file.exists() && file.isFile();
    }

    private boolean isInvalidFileExtension(File file)
    {
        return !isValidFileExtension(file);
    }

    private boolean isValidFileExtension(File file)
    {
        String extension = getFileExtension(file);
        for (String ext : validFileExtensions)
            if (extension.equals(ext))
                return true;
        return false;
    }

    private String getFileExtension(File file)
    {
        String name = file.getName();
        int lastIndexOf = name.lastIndexOf(".");
        if (lastIndexOf == -1)
            return "";

        return name.substring(lastIndexOf);
    }
}
