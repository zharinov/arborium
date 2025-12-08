/**
 * Grammar and host loading for arborium.
 *
 * This module loads the arborium-host WASM component and grammar plugins,
 * implementing the plugin-provider interface that the host imports.
 */

import { createWasiImports, grammarTypesImport } from './wasi-shims.js';
import type { ArboriumConfig } from './types.js';

/** CDN base URLs */
const CDN_URLS: Record<string, string> = {
  jsdelivr: 'https://cdn.jsdelivr.net/npm',
  unpkg: 'https://unpkg.com',
};

/** Default configuration */
export const defaultConfig: Required<ArboriumConfig> = {
  manual: false,
  theme: 'tokyo-night',
  selector: 'pre code, code[data-lang]',
  cdn: 'jsdelivr',
  version: 'latest',
};

/** Get the CDN base URL */
function getCdnUrl(cdn: string): string {
  if (cdn in CDN_URLS) {
    return CDN_URLS[cdn];
  }
  return cdn;
}

/** Get the full URL for a grammar package file */
function getGrammarUrl(
  language: string,
  file: string,
  config: ArboriumConfig
): string {
  const cdn = config.cdn || defaultConfig.cdn;
  const version = config.version || defaultConfig.version;
  const baseUrl = getCdnUrl(cdn);
  const versionSuffix = version === 'latest' ? '' : `@${version}`;
  return `${baseUrl}/@arborium/${language}${versionSuffix}/${file}`;
}

// =============================================================================
// Grammar Plugin Management
// =============================================================================

/** A loaded grammar plugin instance */
interface GrammarPlugin {
  languageId(): string;
  injectionLanguages(): string[];
  createSession(): number;
  freeSession(session: number): void;
  setText(session: number, text: string): void;
  parse(session: number): {
    spans: Array<{ start: number; end: number; capture: string }>;
    injections: Array<{
      start: number;
      end: number;
      language: string;
      includeChildren: boolean;
    }>;
  };
}

/** Cache of loaded grammar plugins by language */
const grammarPlugins = new Map<string, GrammarPlugin>();

/** Cache of in-flight grammar loads */
const loadingGrammars = new Map<string, Promise<GrammarPlugin | null>>();

/** Load a grammar plugin */
async function loadGrammarPlugin(
  language: string,
  config: ArboriumConfig
): Promise<GrammarPlugin | null> {
  // Check cache
  const cached = grammarPlugins.get(language);
  if (cached) return cached;

  // Check in-flight
  const loading = loadingGrammars.get(language);
  if (loading) return loading;

  // Start loading
  const loadPromise = doLoadGrammar(language, config);
  loadingGrammars.set(language, loadPromise);

  try {
    const plugin = await loadPromise;
    if (plugin) {
      grammarPlugins.set(language, plugin);
    }
    return plugin;
  } finally {
    loadingGrammars.delete(language);
  }
}

async function doLoadGrammar(
  language: string,
  config: ArboriumConfig
): Promise<GrammarPlugin | null> {
  try {
    const jsUrl = getGrammarUrl(language, 'grammar.js', config);
    const wasmUrl = getGrammarUrl(language, 'grammar.core.wasm', config);

    const module = await import(/* @vite-ignore */ jsUrl);

    const getCoreModule = async (path: string): Promise<WebAssembly.Module> => {
      const url = path.includes('://') ? path : wasmUrl;
      const response = await fetch(url);
      const bytes = await response.arrayBuffer();
      return WebAssembly.compile(bytes);
    };

    const wasiImports = createWasiImports();
    const imports = {
      ...wasiImports,
      ...grammarTypesImport,
    };

    const instance = await module.instantiate(getCoreModule, imports);
    return instance.plugin as GrammarPlugin;
  } catch (err) {
    console.warn(`[arborium] Failed to load grammar for ${language}:`, err);
    return null;
  }
}

// =============================================================================
// Host Component
// =============================================================================

/** The host component instance */
let hostInstance: HostInterface | null = null;

/** Promise for host loading */
let hostLoadingPromise: Promise<HostInterface | null> | null = null;

/** Interface exported by the host component */
interface HostInterface {
  createDocument(language: string): number | null;
  freeDocument(doc: number): void;
  setText(doc: number, text: string): void;
  highlight(doc: number, maxDepth: number): { html: string };
  getRequiredLanguages(doc: number): string[];
}

/** Plugin handle to plugin instance mapping */
const pluginHandles = new Map<number, GrammarPlugin>();
let nextPluginHandle = 1;

/** Session handle to (plugin, session) mapping */
const sessionHandles = new Map<
  number,
  { plugin: GrammarPlugin; session: number }
>();
let nextSessionHandle = 1;

/** Current config for loading grammars */
let currentConfig: ArboriumConfig = defaultConfig;

/** Load the host component */
async function loadHost(config: ArboriumConfig): Promise<HostInterface | null> {
  if (hostInstance) return hostInstance;
  if (hostLoadingPromise) return hostLoadingPromise;

  currentConfig = config;

  hostLoadingPromise = doLoadHost(config);
  try {
    hostInstance = await hostLoadingPromise;
    return hostInstance;
  } finally {
    hostLoadingPromise = null;
  }
}

async function doLoadHost(
  config: ArboriumConfig
): Promise<HostInterface | null> {
  try {
    const cdn = config.cdn || defaultConfig.cdn;
    const version = config.version || defaultConfig.version;
    const baseUrl = getCdnUrl(cdn);
    const versionSuffix = version === 'latest' ? '' : `@${version}`;

    const jsUrl = `${baseUrl}/@arborium/arborium${versionSuffix}/dist/host.js`;
    const wasmUrl = `${baseUrl}/@arborium/arborium${versionSuffix}/dist/host.core.wasm`;

    const module = await import(/* @vite-ignore */ jsUrl);

    const getCoreModule = async (path: string): Promise<WebAssembly.Module> => {
      const url = path.includes('://') ? path : wasmUrl;
      const response = await fetch(url);
      const bytes = await response.arrayBuffer();
      return WebAssembly.compile(bytes);
    };

    // Implement the plugin-provider interface that the host imports
    const pluginProvider = {
      loadPlugin(language: string): number | null {
        // Check if we have the plugin loaded
        const plugin = grammarPlugins.get(language);
        if (!plugin) return null;

        // Create a handle
        const handle = nextPluginHandle++;
        pluginHandles.set(handle, plugin);
        return handle;
      },

      getInjectionLanguages(pluginHandle: number): string[] {
        const plugin = pluginHandles.get(pluginHandle);
        if (!plugin) return [];
        return plugin.injectionLanguages();
      },

      createPluginSession(pluginHandle: number): number {
        const plugin = pluginHandles.get(pluginHandle);
        if (!plugin) return 0;

        const session = plugin.createSession();
        const handle = nextSessionHandle++;
        sessionHandles.set(handle, { plugin, session });
        return handle;
      },

      freePluginSession(pluginHandle: number, sessionHandle: number): void {
        const sessionInfo = sessionHandles.get(sessionHandle);
        if (sessionInfo) {
          sessionInfo.plugin.freeSession(sessionInfo.session);
          sessionHandles.delete(sessionHandle);
        }
      },

      pluginSetText(
        pluginHandle: number,
        sessionHandle: number,
        text: string
      ): void {
        const sessionInfo = sessionHandles.get(sessionHandle);
        if (sessionInfo) {
          sessionInfo.plugin.setText(sessionInfo.session, text);
        }
      },

      pluginApplyEdit(
        pluginHandle: number,
        sessionHandle: number,
        text: string,
        edit: unknown
      ): void {
        // For now, just set the full text - incremental editing can be added later
        const sessionInfo = sessionHandles.get(sessionHandle);
        if (sessionInfo) {
          sessionInfo.plugin.setText(sessionInfo.session, text);
        }
      },

      pluginParse(
        pluginHandle: number,
        sessionHandle: number
      ): { tag: 'ok'; val: unknown } | { tag: 'err'; val: unknown } {
        const sessionInfo = sessionHandles.get(sessionHandle);
        if (!sessionInfo) {
          return { tag: 'err', val: { message: 'Invalid session' } };
        }

        try {
          const result = sessionInfo.plugin.parse(sessionInfo.session);
          return { tag: 'ok', val: result };
        } catch (err) {
          return {
            tag: 'err',
            val: { message: err instanceof Error ? err.message : String(err) },
          };
        }
      },

      pluginCancel(pluginHandle: number, sessionHandle: number): void {
        // Cancellation not implemented yet
      },
    };

    const wasiImports = createWasiImports();
    const imports = {
      ...wasiImports,
      'arborium:host/plugin-provider': pluginProvider,
    };

    const instance = await module.instantiate(getCoreModule, imports);
    return instance['arborium:host/host'] as HostInterface;
  } catch (err) {
    console.warn('[arborium] Failed to load host:', err);
    return null;
  }
}

// =============================================================================
// Public API
// =============================================================================

/** Load a grammar plugin (preloading for the host) */
export async function loadGrammar(
  language: string,
  config: ArboriumConfig = {}
): Promise<boolean> {
  const mergedConfig = { ...defaultConfig, ...config };
  const plugin = await loadGrammarPlugin(language, mergedConfig);
  return plugin !== null;
}

/** Highlight source code, returning HTML */
export async function highlight(
  language: string,
  source: string,
  config: ArboriumConfig = {}
): Promise<string> {
  const mergedConfig = { ...defaultConfig, ...config };

  // Load the grammar plugin first
  const plugin = await loadGrammarPlugin(language, mergedConfig);
  if (!plugin) {
    throw new Error(`Failed to load grammar for ${language}`);
  }

  // Load the host
  const host = await loadHost(mergedConfig);
  if (!host) {
    // Fall back to direct plugin call without injection support
    const session = plugin.createSession();
    try {
      plugin.setText(session, source);
      const result = plugin.parse(session);
      return spansToHtml(source, result.spans);
    } finally {
      plugin.freeSession(session);
    }
  }

  // Preload injection languages
  const injectionLangs = plugin.injectionLanguages();
  await Promise.all(
    injectionLangs.map((lang) => loadGrammarPlugin(lang, mergedConfig))
  );

  // Use the host for full injection support
  const doc = host.createDocument(language);
  if (doc === null) {
    throw new Error(`Failed to create document for ${language}`);
  }

  try {
    host.setText(doc, source);
    const result = host.highlight(doc, 3); // max depth 3
    return result.html;
  } finally {
    host.freeDocument(doc);
  }
}

/** Convert spans to HTML (fallback when host isn't available) */
export function spansToHtml(
  source: string,
  spans: Array<{ start: number; end: number; capture: string }>
): string {
  if (spans.length === 0) {
    return escapeHtml(source);
  }

  // Sort spans by start position
  const sorted = [...spans].sort((a, b) => a.start - b.start);

  const parts: string[] = [];
  let pos = 0;

  for (const span of sorted) {
    if (span.start > pos) {
      parts.push(escapeHtml(source.slice(pos, span.start)));
    }

    const text = source.slice(span.start, span.end);
    const tag = captureToTag(span.capture);
    parts.push(`<${tag}>${escapeHtml(text)}</${tag}>`);

    pos = span.end;
  }

  if (pos < source.length) {
    parts.push(escapeHtml(source.slice(pos)));
  }

  return parts.join('');
}

/** Map capture names to custom element tags */
function captureToTag(capture: string): string {
  const shortNames: Record<string, string> = {
    keyword: 'a-k',
    string: 'a-s',
    comment: 'a-c',
    function: 'a-f',
    'function.call': 'a-f',
    'function.method': 'a-f',
    type: 'a-t',
    variable: 'a-v',
    'variable.parameter': 'a-v',
    'variable.builtin': 'a-vb',
    number: 'a-n',
    operator: 'a-o',
    punctuation: 'a-p',
    'punctuation.bracket': 'a-p',
    'punctuation.delimiter': 'a-p',
    constant: 'a-ct',
    'constant.builtin': 'a-cb',
    property: 'a-pr',
    attribute: 'a-at',
    tag: 'a-tg',
    namespace: 'a-ns',
    label: 'a-lb',
    escape: 'a-e',
    embedded: 'a-em',
  };

  if (capture in shortNames) {
    return shortNames[capture];
  }

  for (const [prefix, tag] of Object.entries(shortNames)) {
    if (capture.startsWith(prefix + '.')) {
      return tag;
    }
  }

  return 'a-x';
}

/** Escape HTML special characters */
function escapeHtml(text: string): string {
  return text
    .replace(/&/g, '&amp;')
    .replace(/</g, '&lt;')
    .replace(/>/g, '&gt;')
    .replace(/"/g, '&quot;');
}

/** Get the default config merged with user config */
export function getConfig(
  userConfig?: ArboriumConfig
): Required<ArboriumConfig> {
  return { ...defaultConfig, ...userConfig };
}
