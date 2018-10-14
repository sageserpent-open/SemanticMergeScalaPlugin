package scala.meta
import com.sageserpent.neptunium.FileProcessor2.{OneRelativeLineNumber, ZeroRelativeCharacterIndex, ZeroRelativeOffset}

object AccessWorkaround {
  def offsetFrom(input: Input)(line: OneRelativeLineNumber, offset: ZeroRelativeOffset): ZeroRelativeCharacterIndex =
    input.lineToOffset(line - 1) + offset
}
