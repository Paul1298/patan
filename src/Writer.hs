module Writer where
import Graphics.PDF

main :: IO ()
main = do
  let pdfFileName= "test1.pdf"
  let documentInfo = standardDocInfo
  let defaultPageSize = PDFRect 0 0 200 300

  runPdf pdfFileName documentInfo defaultPageSize $ do
    addPage Nothing
