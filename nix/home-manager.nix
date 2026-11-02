{ self }:

{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.programs.handoff;
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

    skills = lib.mkOption {
      type = lib.types.attrsOf lib.types.str;
      readOnly = true;
      default = lib.genAttrs (builtins.attrNames (builtins.readDir ../skills)) (
        name: "${cfg.package}/share/skills/handoff/${name}"
      );
      defaultText = lib.literalMD "each skill's directory in {option}`programs.handoff.package`, by name";
      description = "The skills, for linking them into an agent the module does not integrate with.";
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

    enablePiIntegration = lib.mkOption {
      type = lib.types.bool;
      default = config.programs.pi-coding-agent.enable;
      defaultText = lib.literalExpression "config.programs.pi-coding-agent.enable";
      description = "Whether to link the handoff skills into the `skills` directory of {option}`programs.pi-coding-agent.configDir`.";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ cfg.package ];

    programs.claude-code.skills = lib.mkIf cfg.enableClaudeCodeIntegration cfg.skills;

    programs.codex.skills = lib.mkIf cfg.enableCodexIntegration cfg.skills;

    home.file = lib.mkIf cfg.enablePiIntegration (
      lib.mapAttrs' (
        name: source:
        lib.nameValuePair "${config.programs.pi-coding-agent.configDir}/skills/${name}" { inherit source; }
      ) cfg.skills
    );
  };
}
