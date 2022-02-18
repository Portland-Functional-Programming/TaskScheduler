import { html } from 'lit-html';
import { styleMap } from 'lit-html/directives/style-map.js';
import './button.css';
import './TodoComponent.js'

/**
 * Primary UI component for user interaction
 */
export const Button = ({ primary, backgroundColor = null, size, label, onClick }) => {
  const mode = primary ? 'storybook-button--primary' : 'storybook-button--secondary';

  return html`
    <halogen-todo>
    </halogen-todo>
  `;
};
