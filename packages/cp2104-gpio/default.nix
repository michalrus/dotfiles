{ python3, runCommand }:

with python3.pkgs;

buildPythonApplication {
  name = "cp2104-gpio";
  pyproject = true;
  build-system = [ setuptools ];
  src = runCommand "src" {} ''
    mkdir $out

    cp ${./cp2104-gpio.py} $out/cp2104-gpio

    cat << EOF > $out/setup.py
    from distutils.core import setup
    setup(scripts=['cp2104-gpio'], install_requires=['pyusb'])
    EOF
  '';
  propagatedBuildInputs = [ pyusb ];
}
