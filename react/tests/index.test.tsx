import React from 'react';
import TimeSelector from '../src';
import { render } from '@testing-library/react';

test('Renders TimeSelector', () => {
    const { getByPlaceholderText } = render(<TimeSelector/>);
    const input = getByPlaceholderText('type a time and select below');
    expect(input).toBeInTheDocument();
})
