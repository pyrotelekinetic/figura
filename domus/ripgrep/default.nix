{ pkgs, ... }: {

wrappers.ripgrep = {
  basePackage = pkgs.ripgrep;
  # Use config file instead of wrapping flags directly so that `--no-config` can work
  env.RIPGREP_CONFIG_PATH.value = pkgs.writeText "ripgreprc" ''
    --smart-case
  '';
  postBuild = ''
    echo "checking config"
    # `rg` exits with 1 when no matches are found, and 2 on error so, if the configuration has no errors, we should get exit code 1
    $out/bin/rg test /dev/null || [ $? -eq 1 ]
  '';
};

}
