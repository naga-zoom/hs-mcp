# MCP-Haskell (hs-mcp)

A Haskell implementation of the [Model Context Protocol (MCP)](https://modelcontextprotocol.io/).

**NOTE**: This is a fork of [Bryan Buecking](https://github.com/buecking/hs-mcp)'s original implementation.

I've changed a lot in the interface, error handling, and also expose a simple `handleMessage` primitive which allows using an MCP server via an http interface, not just stdio.

## Overview

MCP-Haskell (hs-mcp) provides a Haskell implementation of the Model Context Protocol, allowing Haskell applications to expose tools, resources, and prompts to MCP-compatible clients like Claude.

Key features:

- Full implementation of MCP protocol
- StdIO transport for local process communication
- JSON-RPC messaging
- Support for resources, tools, and prompts
- Comprehensive test suite

## Installation

```bash
stack build
```

## Example Server

The project includes an example echo server that demonstrates the MCP functionality:

```bash
# Build and run the example server
stack run mcp-echo-server
```

You can test it with the [MCP Inspector](https://github.com/modelcontextprotocol/inspector) or Claude Desktop.

## Testing

Run the test suite:

```bash
stack test
```

## Protocol Compatibility

This implementation follows the [Model Context Protocol specification](https://spec.modelcontextprotocol.io/) and is compatible with:

- Claude Desktop
- MCP Inspector
- Other MCP clients following the specification

## Project Structure

- `src/Network/MCP/Types.hs` - Core MCP types
- `src/Network/MCP/Transport/` - Transport implementations
- `src/Network/MCP/Server/` - Server implementation
- `Examples/` - Example implementations
- `Test/` - Test suite

## License

This project is licensed under the BSD-3-Clause License - see the LICENSE file for details.

## Structured result schemas

`Tool.toolOutputSchema` optionally declares the JSON Schema for a tool's
`CallToolResult.structuredContent`, as specified by
[MCP 2025-06-18](https://modelcontextprotocol.io/specification/2025-06-18/server/tools#output-schema).
Use `Nothing` for tools without a declaration and `Just schema` for structured
results. Discovery preserves the schema object; this library does not evaluate
JSON Schema or enforce result conformance. Servers must return matching structured
results and clients should validate them.

Existing Haskell `Tool` constructions must initialize the new optional field;
positional constructions take it after `toolAnnotations`. Older wire definitions
without `outputSchema` remain readable and omit the field when re-encoded.
