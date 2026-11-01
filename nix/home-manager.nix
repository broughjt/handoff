{ self }:

{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.programs.handoff;

  skills = lib.genAttrs (builtins.attrNames (builtins.readDir ../skills)) (
    name: "${cfg.package}/share/skills/handoff/${name}"
  );
in
{
  options.programs.handoff = {
    enable = lib.mkEnableOption "handoff, coding agent skills for context management across many sessions";

    package = lib.mkOption {
      type = lib.types.package;
      default = self.packages.${pkgs.stdenv.hostPlatform.system}.handoff;
      defaultText = lib.literalExpression "handoff.packages.\${pkgs.stdenv.hostPlatform.system}.handoff";
      description = "The handoff package, which provides the `handoff` command and the skills.";
    };

    enableClaudeCodeIntegration = lib.mkOption {
      type = lib.types.bool;
      default = config.programs.claude-code.enable;
      defaultText = lib.literalExpression "config.programs.claude-code.enable";
      description = "Whether to add the handoff skills to {option}`programs.claude-code.skills`.";
    };

    enableCodexIntegration = lib.mkOption {
      type = lib.types.bool;
      default = config.programs.codex.enable;
      defaultText = lib.literalExpression "config.programs.codex.enable";
      description = "Whether to add the handoff skills to {option}`programs.codex.skills`.";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ cfg.package ];

    programs.claude-code.skills = lib.mkIf cfg.enableClaudeCodeIntegration skills;

    programs.codex.skills = lib.mkIf cfg.enableCodexIntegration skills;
  };
}
