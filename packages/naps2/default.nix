{
  lib,
  naps2,
  tesseract,
}:
# XXX: the original package has wrong Tesseract linkage
naps2.overrideAttrs (drv: {
  postInstall =
    (drv.postInstall or "")
    + ''
      chmod -R +w $out
      rm $out/lib/naps2/_linux/tesseract
      ln -s ${lib.getExe tesseract} $out/lib/naps2/_linux/tesseract
    '';
})
