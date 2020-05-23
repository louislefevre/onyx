package ui;

import javafx.stage.FileChooser;
import javafx.stage.Stage;
import lombok.Getter;
import lombok.Setter;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.PrintWriter;
import java.nio.file.AccessDeniedException;
import java.nio.file.Files;

@Getter
@Setter
final class FileManager
{
    private File initialDirectory;
    private String[] validFileExtensions;
    private File currentFile;
    private String originalText;

    public FileManager(File initialDirectory, String... validFileExtensions)
    {
        this.initialDirectory = initialDirectory;
        this.validFileExtensions = validFileExtensions;
        this.currentFile = null;
        this.originalText = null;
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
        else if (isNonExistentFile(file))
            throw new FileNotFoundException("Selected file doesn't exist: " + file.getPath());
        else if (isInvalidReadFile(file))
            throw new AccessDeniedException("Access denied - you do not have permission to read this file");
        else if (isInvalidFileExtension(file))
            throw new IllegalArgumentException("Invalid file extension");
        else
            return readFile(file);
    }

    public void saveFileAs(String text) throws IOException
    {
        String initialFileName = "new-script.txt";
        saveFileAs(text, initialFileName);
    }

    public void saveFileAs(String text, String initialFileName) throws IOException, NullPointerException, IllegalArgumentException
    {
        FileChooser fileChooser = new FileChooser();
        addInitialDirectory(fileChooser);
        addExtensionFilters(fileChooser);
        fileChooser.setInitialFileName(initialFileName);

        File file = fileChooser.showSaveDialog(new Stage());
        if (file == null)
            throw new NullPointerException("No file was selected");
        else if (isNonExistentFile(file))
            throw new FileNotFoundException("Selected file doesn't exist: " + file.getPath());
        else if (isInvalidWriteFile(file))
            throw new AccessDeniedException("Access denied - you do not have permission to write to this file");
        else if (isInvalidFileExtension(file))
            throw new IllegalArgumentException("Invalid file extension");
        else
            writeFile(file, text);
    }

    public void saveFile(String text) throws IOException
    {
        File file = currentFile;
        if (isValidSaveFile(file))
            writeFile(file, text);
        else
            saveFileAs(text);
    }

    public boolean checkIfSaved(String text)
    {
        if (originalText == null)
            return true;
        return originalText.equals(text);
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
        originalText = fileText;
        return fileText;
    }

    private void writeFile(File file, String text) throws FileNotFoundException
    {
        PrintWriter writer = new PrintWriter(file);
        writer.print(text);
        writer.close();
        currentFile = file;
        originalText = text;
    }

    private boolean isNonExistentFile(File file)
    {
        return !file.exists();
    }

    private boolean isInvalidReadFile(File file)
    {
        return !file.canRead();
    }

    private boolean isInvalidWriteFile(File file)
    {
        return !file.canWrite();
    }

    private boolean isValidSaveFile(File file)
    {
        return file != null && file.exists() && file.isFile() && file.canWrite() && isValidFileExtension(file);
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
