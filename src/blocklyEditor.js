// Import Blockly core.
import * as Blockly from 'blockly/core';
// Import the default blocks.
import * as libraryBlocks from 'blockly/blocks';
// Import a generator.
import { javascriptGenerator } from 'blockly/javascript';
// Import a message file.
import * as En from 'blockly/msg/en';

Blockly.setLocale(En);

const blocks = Blockly.common.createBlockDefinitionsFromJsonArray([
    {
        "type": "smol_defvar",
        "message0": "var %1 =%2",
        "args0": [
            {
                "type": "field_input",
                "name": "VAR_NAME",
                "text": ""
            },
            {
                "type": "input_value",
                "name": "VAR_INIT"
            }
        ],
        "previousStatement": null,
        "nextStatement": null,
        "colour": 230,
    },
    {
        "type": "smol_deffun",
        "message0": "fun %1(%2):",
        "args0": [
            {
                "type": "field_input",
                "name": "FUN_NAME",
                "text": ""
            },
            {
                "type": "input_value",
                "name": "FUN_ARGS",
                "inputsInline": true
            }
        ],
        "message1": "%1end",
        "args1": [
            {
                "type": "input_statement",
                "name": "BODY"
            }
        ],
        "previousStatement": null,
        "nextStatement": null,
        "colour": 230,
    },
    {
        "type": "smol_argument",
        "message0": "%1",
        "args0": [{
            "type": "field_input",
            "name": "NAME"
        }],
        "output": null
    }
//     {
//     "type": "object",
//     "message0": "{ %1 %2 }",
//     "args0": [
//         {
//             "type": "input_dummy"
//         },
//         {
//             "type": "input_statement",
//             "name": "MEMBERS"
//         }
//     ],
//     "output": null,
//     "colour": 230,
// },
// {
//     "type": "member",
//     "message0": "%1 %2 %3",
//     "args0": [
//         {
//             "type": "field_input",
//             "name": "MEMBER_NAME",
//             "text": ""
//         },
//         {
//             "type": "field_label",
//             "name": "COLON",
//             "text": ":"
//         },
//         {
//             "type": "input_value",
//             "name": "MEMBER_VALUE"
//         }
//     ],
//     "previousStatement": null,
//     "nextStatement": null,
//     "colour": 230,
// }
]);

// TODO: https://github.com/google/blockly/blob/9a7de53029afc09dfc9d055a4eae40b316728455/blocks/lists.ts#L125C1-L288C3

const toolbox = {
    // There are two kinds of toolboxes. The simpler one is a flyout toolbox.
    kind: 'flyoutToolbox',
    // The contents is the blocks and other items that exist in your toolbox.
    contents: [
        {
            kind: 'block',
            type: 'controls_if'
        },
        {
            kind: 'block',
            type: 'controls_whileUntil'
        },
        // You can add more blocks to this array.
        {
            'kind': 'block',
            'type': 'smol_argument'
        },
        {
            'kind': 'block',
            'type': 'smol_defvar'
        },
        {
            'kind': 'block',
            'type': 'smol_deffun'
        },
        // {
        //     'kind': 'block',
        //     'type': 'object'
        // },
        // {
        //     'kind': 'block',
        //     'type': 'member'
        // },
        {
            'kind': 'block',
            'type': 'math_number'
        },
        {
            'kind': 'block',
            'type': 'text'
        },
        {
            'kind': 'block',
            'type': 'logic_boolean'
        },
        {
            'kind': 'block',
            'type': 'logic_null'
        },
        {
            'kind': 'block',
            'type': 'lists_create_with'
        }
    ]
};

// Create the definition.
// const definitions = Blockly.common.createBlockDefinitionsFromJsonArray([
//     {
//         // The type is like the "class name" for your block. It is used to construct
//         // new instances. E.g. in the toolbox.
//         type: 'my_custom_block',
//         // The message defines the basic text of your block, and where inputs or
//         // fields will be inserted.
//         message0: 'move forward %1',
//         args0: [
//             // Each arg is associated with a %# in the message.
//             // This one gets substituted for %1.
//             {
//                 // The type specifies the kind of input or field to be inserted.
//                 type: 'field_number',
//                 // The name allows you to reference the field and get its value.
//                 name: 'FIELD_NAME',
//             }
//         ],
//         // Adds an untyped previous connection to the top of the block.
//         previousStatement: null,
//         // Adds an untyped next connection to the bottom of the block.
//         nextStatement: null,
//     }
// ]);

// Register the definition.
Blockly.common.defineBlocks(blocks);

// Passes the injection div.
const workspace = Blockly.inject(
    document.getElementById('blocklyDiv'), { toolbox });
